//
//  MWBCommunicator.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 11..
//  Copyright © 2017. theMatys. All rights reserved.
//

import Foundation
import CoreBluetooth

//Class for handling connections to myWatch Bluetooth devices.
//The class you instantiate this class in, must conform to the protocol MWBCommunicatorDelegate.
//The conforming class must implement the callback methods of the protocol.
class MWBCommunicator: NSObject, CBCentralManagerDelegate, CBPeripheralDelegate
{
    //MARK: Member variables
    private var delegate: MWBCommunicatorDelegate?
    {
        willSet(newDelegate)
        {
            if(delegate != nil && newDelegate == nil)
            {
                deinitializeBluetooth()
            }
        }
    }
    
    //MARK: Member variables - Bluetooth-related variables
    private var centralManager: CBCentralManager? = nil
    private var peripheral: CBPeripheral? = nil
    private var device: MWDevice? = nil
    
    private var uartTxCharacteristic: CBCharacteristic? = nil
    private var uartRxCharacteristic: CBCharacteristic? = nil
    
    private var commandQueue: [MWBCommand] = [MWBCommand]()
    
    //MARK: Member variables - Constant defaults
    private let uartSericeUUID: CBUUID = CBUUID(string: "6E400001-B5A3-F393-E0A9-E50E24DCCA9E")
    private let deviceDescUUID: CBUUID = CBUUID(string: "1803")
    private let uartTxUUID: CBUUID = CBUUID(string: "6E400002-B5A3-F393-E0A9-E50E24DCCA9E")
    private let uartRxUUID: CBUUID = CBUUID(string: "6E400003-B5A3-F393-E0A9-E50E24DCCA9E")
    
    //MARK: - Instance functions
    override init()
    {
        super.init()
    }
    
    func initializeBluetooth(withDelegate delegate: MWBCommunicatorDelegate)
    {
        self.delegate = delegate
        centralManager = CBCentralManager.init(delegate: self, queue: nil, options: nil)
    }
    
    func initializeBluetooth(withDelegate delegate: MWBCommunicatorDelegate, withDevice device: MWDevice)
    {
        self.delegate = delegate
        self.device = device
        centralManager = CBCentralManager.init(delegate: self, queue: nil, options: nil)
    }
    
    func lookForDevices()
    {
        centralManager!.scanForPeripherals(withServices: nil, options: nil)
    }
    
    func attemptToConnect(to device: MWDevice)
    {
        self.device = device
        self.peripheral = device.peripheral
        self.peripheral!.delegate = self
        
        centralManager!.connect(self.peripheral!, options: nil)
    }
    
    func sendCommand(command: MWBCommand)
    {
        if(uartTxCharacteristic != nil)
        {
            commandQueue.append(command)
            self.peripheral!.writeValue(command.getCommandData(), for: uartTxCharacteristic!, type: .withResponse)
        }
    }
    
    func changeDelegate(to delegate: MWBCommunicatorDelegate)
    {
        self.delegate = delegate
    }
    
    func deinitializeBluetooth()
    {
        self.peripheral!.setNotifyValue(false, for: uartRxCharacteristic!)
        self.centralManager!.cancelPeripheralConnection(peripheral!)
        
        self.centralManager = nil
        self.peripheral = nil
        self.device = nil
        self.uartTxCharacteristic = nil
        self.commandQueue = [MWBCommand]()
        
        MWLInfo("Bluetooth has been deinitialized.", module: .moduleBluetooth)
    }
    
    //MARK: Inherited functions from: CBCentralManagerDelegate
    internal func centralManagerDidUpdateState(_ central: CBCentralManager)
    {
        if(central.state == .poweredOn)
        {
            MWLInfo("Bluetooth has been turned on.", module: .moduleBluetooth)
            
            if(device != nil)
            {
                lookForDevices()
            }
            else
            {
                if(self.delegate!.bluetoothHasBeenEnabled != nil)
                {
                    self.delegate!.bluetoothHasBeenEnabled!()
                }
                else
                {
                    MWLInfo("WARNING: Initializing without specified device, but function \"bluetoothHasBeenEnabled()\" is not implemented in the current delegate.", module: .moduleBluetooth)
                }
            }
        }
        else
        {
            MWLInfo("Bluetooth is not available at the moment.", module: .moduleBluetooth)
            
            MWUtil.execute(ifNotNil: self.delegate!.bluetoothNotAvailable, execution: { 
                self.delegate!.bluetoothNotAvailable!()
            }, elseExecution: { 
                MWLInfo("WARNING: Bluetooth is not available, but function \"bluetoothNotAvailable()\" is not implemented in the current delegate.", module: .moduleBluetooth)
            })
        }
    }
    
    internal func centralManager(_ central: CBCentralManager, didDiscover peripheral: CBPeripheral, advertisementData: [String : Any], rssi RSSI: NSNumber)
    {
        if(peripheral.name == MWDefaults.Bluetooth.defaultDeviceName)
        {
            let unparsed: String = String(describing: advertisementData["kCBAdvDataManufacturerData"]!)
            let parsed = parseDeviceID(unparsed)
            let device = MWDevice(givenName: "", deviceID: parsed, peripheral: peripheral)
            
            if(self.device != nil)
            {
                if(self.device!.deviceID == device.deviceID)
                {
                    if(delegate!.bluetoothHasFoundSpecifiedDevice != nil)
                    {
                        delegate!.bluetoothHasFoundSpecifiedDevice!()
                        self.device!.peripheral = peripheral
                        
                        attemptToConnect(to: self.device!)
                    }
                    else
                    {
                        MWLInfo("WARNING: Found device matching the device ID of the MWDevice specified in the initializer, but function \"bluetoothHasFoundSpecifiedDevice()\" is not implemented in the current delegate.", module: .moduleBluetooth)
                    }
                }
            }
            else
            {
                if(self.delegate!.bluetoothHasFoundDevice != nil)
                {
                    self.delegate!.bluetoothHasFoundDevice!(device)
                }
                else
                {
                    MWLInfo("WARNING: Found device matching the myWatch device requirements, but function \"bluetoothHasFoundDevice(_:)\" is not implemented in the current delegate.", module: .moduleBluetooth)
                }
            }
        }
    }
    
    internal func centralManager(_ central: CBCentralManager, didConnect peripheral: CBPeripheral)
    {
        MWLInfo("Successfully connected to peripheral: \"\(peripheral.name!)\"", module: .moduleBluetooth)
        
        if(self.delegate!.connectionSuccessful != nil)
        {
            self.delegate!.connectionSuccessful!(to: self.device!)
        }
        else
        {
            MWLInfo("WARNING: Function \"connectionSuccessful(to:)\" is not implemented in the current delegate.", module: .moduleBluetooth)
        }
            
        self.centralManager!.stopScan()
        peripheral.discoverServices([uartSericeUUID])
    }
    
    internal func centralManager(_ central: CBCentralManager, didFailToConnect peripheral: CBPeripheral, error: Error?)
    {
        MWLError("Was unable to connect to peripheral: \"\(peripheral.name!)\"", module: .moduleBluetooth)
    }
    
    //MARK: Inherited functions from: CBPeripheralDelegate
    internal func peripheral(_ peripheral: CBPeripheral, didDiscoverServices error: Error?)
    {
        if(peripheral.services != nil)
        {
            for service in peripheral.services!
            {
                if(service.uuid == uartSericeUUID)
                {
                    peripheral.discoverCharacteristics([uartTxUUID, uartRxUUID], for: service)
                }
            }
        }
        else
        {
            MWLError("Was unable to discover the Uart service on the connected myWatch device.", module: .moduleBluetooth)
        }
    }
    
    internal func peripheral(_ peripheral: CBPeripheral, didDiscoverCharacteristicsFor service: CBService, error: Error?)
    {
        if(service.uuid == uartSericeUUID && service.characteristics != nil)
        {
            for characteristic in service.characteristics!
            {
                if(characteristic.uuid == uartTxUUID)
                {
                    self.uartTxCharacteristic = characteristic
                }
                else if(characteristic.uuid == uartRxUUID)
                {
                    self.uartRxCharacteristic = characteristic
                    peripheral.setNotifyValue(true, for: characteristic)
                }
                else
                {
                    MWLError("Found useless characteristic with UUID: \(characteristic.uuid) on the connected myWatch device, ignoring it...", module: .moduleBluetooth)
                }
            }
        }
        else
        {
            MWLError("Was unable to discover characteristics on the connected myWatch device.", module: .moduleBluetooth)
        }
        
        if(uartTxCharacteristic != nil && uartRxCharacteristic != nil)
        {
            if(self.delegate!.deviceIsReadyToUse != nil)
            {
                MWLInfo("The connected myWatch device is ready tp use.", module: .moduleBluetooth)
                self.delegate!.deviceIsReadyToUse!(self.device!)
            }
            else
            {
                MWLInfo("WARNING: Function \"deviceIsReadyToUse(_:)\" is not implemented in the current delegate.", module: .moduleBluetooth)
            }
        }
    }
    
    internal func peripheral(_ peripheral: CBPeripheral, didUpdateValueFor characteristic: CBCharacteristic, error: Error?)
    {
        if(commandQueue.count > 1 && commandQueue[0].isLastExecuted())
        {
            commandQueue.remove(at: 0)
        }
            
        if(characteristic.uuid == uartRxUUID && characteristic.value != nil)
        {
            MWLInfo("Data received from Uart Rx characteristic as a response to command: \(commandQueue[0].getCommand())", module: .moduleBluetooth)
        
            let response: MWBParsedData = MWBParsedData(unparsed: characteristic.value!)
            
            if(self.delegate!.recievedResponse != nil)
            {
                self.delegate!.recievedResponse!(forCommand: commandQueue[0].getCommand(), response: response)
            }
            else
            {
                MWLInfo("WARNING: Function \"receivedResponse(forCommand:response:)\" is not implemented in the current delegate.", module: .moduleBluetooth)
            }
        }
        else if(characteristic.uuid == uartRxUUID && characteristic.value == nil)
        {
            MWLError("Received nil from Uart Rx characeteristic as a response to command: \(commandQueue[0].getCommand())", module: .moduleBluetooth)
        }
            
        if(commandQueue.count == 1)
        {
            commandQueue[0].setLastExecuted()
        }
        else if(commandQueue.count > 1)
        {
            commandQueue.remove(at: 0)
        }
    }
    
    internal func peripheral(_ peripheral: CBPeripheral, didWriteValueFor characteristic: CBCharacteristic, error: Error?)
    {
        if(characteristic == uartTxCharacteristic && commandQueue.count >= 1)
        {
            MWLInfo("Successfully delivered command: \(commandQueue[0].getCommand()) to the connected myWatch device.", module: .moduleBluetooth)
        
            if(commandQueue[0].isNonResponse())
            {
                commandQueue.remove(at: 0)
            }
        }
    }
    
    //MARK: Private functions
    private func parseDeviceID(_ unparsed: String) -> String
    {
        var reachedIDStart: Bool = false
        var buffer: String = ""
        var parsed: String = ""
        
        for character in unparsed.characters
        {
            if(character == "1" && !reachedIDStart)
            {
                reachedIDStart = true
                continue
            }
            
            if((character != "<" && character != ">" && character != " ") && reachedIDStart)
            {
                if(buffer.characters.count == 0)
                {
                    buffer = String(character)
                }
                else if(buffer.characters.count == 1)
                {
                    buffer.append(character)
                    
                    parsed = buffer + parsed
                    
                    buffer = ""
                }
            }
        }
        
        return parsed.uppercased()
    }
}

//MARK: -
class MWBCommand
{
    //MARK: Member variables
    private var command: String
    private var commandData: Data
    private var nonResponse: Bool
    private var lastExecuted: Bool = false
    
    //MARK: - Instance functions
    init(command: String, nonResponse: Bool)
    {
        self.command = command
        self.nonResponse = nonResponse
        
        self.commandData = command.data(using: .utf8)!
    }
    
    convenience init(command: String)
    {
        self.init(command: command, nonResponse: false)
    }
    
    func getCommand() -> String
    {
        return self.command
    }
    
    func getCommandData() -> Data
    {
        return self.commandData
    }
    
    func isNonResponse() -> Bool
    {
        return self.nonResponse
    }
    
    func setLastExecuted()
    {
        self.lastExecuted = true
    }
    
    func isLastExecuted() -> Bool
    {
        return self.lastExecuted
    }
}

//MARK: -
@objc class MWBParsedData: NSObject
{
    //MARK: Member variables
    private var dataString: String = "<NIL>"
    private var dataNumeric: Int = -1
    
    //MARK: - Instance methods
    init(unparsed data: Data)
    {
        super.init()
        
        parseStringData(data)
        parseNumericData(data)
    }
    
    func getStringData() -> String
    {
        return self.dataString
    }
    
    func getNumericData() -> Int
    {
        return self.dataNumeric
    }
    
    private func parseStringData(_ data: Data)
    {
        let parsed: String? = String(data: data, encoding: .utf8)
        
        if(parsed != nil)
        {
            self.dataString = parsed!
        }
    }
    
    private func parseNumericData(_ data: Data)
    {
        let parsed: Int? = Int(exactly: UInt64(bigEndian: data.withUnsafeBytes { $0.pointee }))
        
        if(parsed != nil)
        {
            self.dataNumeric = parsed!
        }
    }
}

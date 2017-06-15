//
//  MWBCommunicatorDelegate.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 12..
//  Copyright © 2017. theMatys. All rights reserved.
//

import CoreBluetooth

//Protocol for classes which want Bluetooth functionality.
//When conformed, the class has to handle the callback functions declared below.
@objc protocol MWBCommunicatorDelegate
{
    //MARK: Protocol functions
    
    //Callback function. Called when the CBCentralManager in MWBCommunicator updates its state to '.poweredOn'.
    @objc optional func bluetoothHasBeenEnabled()
    
    //Callback function. Called when function 'centralManagerDidUpdateState' function gave a state which is not '.poweredOn'.
    @objc optional func bluetoothNotAvailable()
    
    //Callback function. Called when the CBCentralManager in MWBCommunicator found and constructed a myWatch device.
    @objc optional func bluetoothHasFoundDevice(_ device: MWDevice)
    
    //Callback function. Called when the MWBCommunicator has found the specified device in the initializer.
    @objc optional func bluetoothHasFoundSpecifiedDevice()
    
    //Callback function. Called when the MWBCommunicator successfully connected to a myWatch device.
    @objc optional func connectionSuccessful(to device: MWDevice)
    
    //Callback function. Called when both the Tx and Rx characteristics were discovered by MWBCommunicator.
    @objc optional func deviceIsReadyToUse(_ device: MWDevice)
    
    //Callback function. Called when the Rx characteristic's value changes as a response to the previously sent command.
    @objc optional func recievedResponse(forCommand command: String, response data: MWBParsedData)
}

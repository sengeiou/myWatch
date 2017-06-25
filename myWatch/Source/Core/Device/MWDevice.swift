//
//  MWDevice.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 12..
//  Copyright © 2017. theMatys. All rights reserved.
//

import CoreBluetooth

/// Represents the myWatch device that the application uses.
///
/// Step/sleep data are held in this object as well as the given name, device ID and the peripheral repsresenting this device.
///
/// Encoded to a file when the app is about to terminate to presist data for the device.
@objc class MWDevice: NSObject, NSCoding
{
    //MARK: Instance variables
    
    /// The name of the device given by the user.
    var givenName: String
    
    /// The device's ID which is used to identify the device after first launch.
    var deviceID: String
    
    /// Temporary variable holding the current step count for this device for the current day.
    var stepCount: Int = 0
    
    /// The Bluetooth peripheral representing this device.
    var peripheral: CBPeripheral?
    
    //MARK: - Inherited initializers from: NSCoding
    required convenience init?(coder aDecoder: NSCoder)
    {
        let givenName: String? = aDecoder.decodeObject(forKey: PropertyKey.givenName) as? String
        let deviceID: String? = aDecoder.decodeObject(forKey: PropertyKey.deviceID) as? String
        let stepCount: Int = aDecoder.decodeInteger(forKey: PropertyKey.stepCount)
        
        self.init(givenName: givenName ?? "", deviceID: deviceID ?? "", peripheral: nil, stepCount: stepCount)
    }
    
    //MARK: Initializers
    
    /// Makes an `MWDevice` object.
    ///
    /// Can be called programatically by any class or by `init(coder:)` to initialize.
    ///
    /// - Parameters:
    ///   - givenName: The name of this device given by the user.
    ///   - deviceID: The ID used to identify this device for later use.
    ///   - peripheral: The Bluetooth peripheral representing this device object.
    ///
    /// - Returns: An `MWDevice` object.
    init(givenName: String, deviceID: String, peripheral: CBPeripheral?, stepCount: Int)
    {
        self.givenName = givenName
        self.deviceID = deviceID
        self.peripheral = peripheral
        self.stepCount = stepCount
    }
    
    //MARK: Inherited functions from: NSCoding
    func encode(with aCoder: NSCoder)
    {
        aCoder.encode(givenName, forKey: PropertyKey.givenName)
        aCoder.encode(deviceID, forKey: PropertyKey.deviceID)
        aCoder.encode(stepCount, forKey: PropertyKey.stepCount)
    }
    
    //MARK: -
    
    /// The structure which holds the property names used in the files to identify the properties of this object.
    private struct PropertyKey
    {
        static let givenName: String = "MWPDeviceGivenName"
        static let deviceID: String = "MWPDeviceID"
        static let stepCount: String = "MWPDeviceStepCount"
    }
}

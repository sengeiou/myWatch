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
    var name: String
    
    /// The device's ID which is used to identify the device after first launch.
    var identifier: String
    
    /// The Bluetooth peripheral representing this device.
    var peripheral: CBPeripheral?
    
    //MARK: - Inherited initializers from: NSCoding
    required convenience init?(coder aDecoder: NSCoder)
    {
        let name: String = aDecoder.decodeObject(forKey: PropertyKey.name) as? String ?? ""
        let identifier: String = aDecoder.decodeObject(forKey: PropertyKey.identifier) as? String ?? ""
        
        self.init(name: name, identifier: identifier)
    }
    
    //MARK: Initializers
    
    /// Makes an `MWDevice` object.
    ///
    /// Can be called programatically by any class or by `init(coder:)` to initialize.
    ///
    /// - Parameters:
    ///   - name: The name of this device given by the user.
    ///   - identifier: The ID used to identify this device for later use.
    ///   - peripheral: The Bluetooth peripheral representing this device object.
    ///
    /// - Returns: An `MWDevice` object.
    init(name: String = "", identifier: String = "", peripheral: CBPeripheral? = nil)
    {
        self.name = name
        self.identifier = identifier
        self.peripheral = peripheral
    }
    
    //MARK: Inherited functions from: NSCoding
    func encode(with aCoder: NSCoder)
    {
        aCoder.encode(name, forKey: PropertyKey.name)
        aCoder.encode(identifier, forKey: PropertyKey.identifier)
    }
    
    //MARK: -
    
    /// The structure which holds the property names used in the files to identify the properties of this object.
    private struct PropertyKey
    {
        //MARK: Prefixes
        
        /// The prefix of the property keys.
        private static let prefix: String = "MWDevice"
        
        //MARK: Property keys
        static let name: String = prefix + "Name"
        static let identifier: String = prefix + "Identifier"
    }
}

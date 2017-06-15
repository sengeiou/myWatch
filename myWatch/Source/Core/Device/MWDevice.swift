//
//  MWDevice.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 12..
//  Copyright © 2017. theMatys. All rights reserved.
//

import CoreBluetooth

@objc class MWDevice: NSObject, NSCoding
{
    //MARK: Member variables
    var givenName: String
    var deviceID: String
    var stepCount: Int = 0
    var peripheral: CBPeripheral?
    
    //MARK: - Instance functions
    init(givenName: String, deviceID: String, peripheral: CBPeripheral?)
    {
        self.givenName = givenName
        self.deviceID = deviceID
        self.peripheral = peripheral
    }
    
    //MARK: Instance encoding functions
    required convenience init?(coder aDecoder: NSCoder)
    {
        let givenName: String? = aDecoder.decodeObject(forKey: PropertyKey.PROPERTY_KEY_GIVEN_NAME) as? String
        let deviceID: String? = aDecoder.decodeObject(forKey: PropertyKey.PROPERTY_KEY_DEVICE_ID) as? String
        
        self.init(givenName: givenName != nil ? givenName! : "", deviceID: deviceID != nil ? deviceID! : "", peripheral: nil)
    }
    
    func encode(with aCoder: NSCoder)
    {
        aCoder.encode(givenName, forKey: PropertyKey.PROPERTY_KEY_GIVEN_NAME)
        aCoder.encode(deviceID, forKey: PropertyKey.PROPERTY_KEY_DEVICE_ID)
        aCoder.encode(stepCount, forKey: PropertyKey.PROPERTY_KEY_STEP_COUNT)
    }
    
    //MARK: Property keys structure
    private struct PropertyKey
    {
        static let PROPERTY_KEY_GIVEN_NAME: String = "MWPDeviceGivenName"
        static let PROPERTY_KEY_DEVICE_ID: String = "MWPDeviceID"
        static let PROPERTY_KEY_STEP_COUNT: String = "MWPDeviceStepCount"
    }
}

//
//  MWIdentifiers.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 09..
//  Copyright © 2017. theMatys. All rights reserved.
//

import Foundation

//MARK: Prefixes

/// The prefix used for all segue identifiers.
fileprivate let prefixSegueID = "MWSID"

/// The prefix used for all table view cell identifiers.
fileprivate let prefixCellID = "MWCID"

/// Holds all the identifiers in the application that are used programatically.
struct MWIdentifiers
{
    //MARK: -
    
    /// The table view cell identifiers used programatically in the application.
    struct CellIdentifiers
    {
        static let deviceChooserDeviceCell = prefixCellID + "DeviceChooserDeviceCell"
    }
    
    //MARK: -
    
    /// The table view cell identifiers used programatically in the application.
    struct TableViewCellIdentifiers
    {
        static let deviceChooserDevice: String = "MWDeviceChooserDeviceTableViewCell"
    }
    
    //MARK: -
    
    /// The segue identifiers used programatically in the application.
    struct SegueIdentifiers
    {
        static let connectingToNameDevice = prefixSegueID + "ConnectingToNameDevice"
        
        static let appleHealthToFirstLaunchLast = "MWAppleHealthToFirstLaunchLastSegue"
        static let deviceChooserToNameDevice = "MWDeviceChooserToNameDeviceSegue"
    }
    
    //MARK: -
    
    /// The controller identifiers used programatically in the application.
    ///
    /// The `VC` postfix at the end of a property's name indicates that the property is a view controller's identifier.
    ///
    /// The `NC` postfix at the end of a property's name indicates that the property is a navigation controller's identifier.
    ///
    /// The `TC` postfix at the end of a property's name indicates that the property is a tab bar controller's identifier.
    ///
    /// The `TVC` postfix at the end of a property's name indicates that the property is a table view controller's identifier.
    struct ControllerIdentifiers
    {
        static let firstLaunchNC: String = "MWFirstLaunchNavigationController"
    }
}

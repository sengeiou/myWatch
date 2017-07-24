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

/// The prefix used for all scene identifiers.
fileprivate let prefixSceneID = "MWSCID"

/// The prefix used for all table view cell identifiers.
fileprivate let prefixCellID = "MWCID"

/// The prefix used for all table view cell identifiers.
fileprivate let prefixTableViewCell: String = "MWTableViewCell"

/// The prefix used for all segue identifiers.
fileprivate let prefixSegue: String = "MWSegue"

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
        static let deviceChooserDevice: String = prefixTableViewCell + "DeviceChooserDevice"
    }
    
    //MARK: -
    
    /// The segue identifiers used programatically in the application.
    struct SegueIdentifiers
    {
        static let connectingToNameDevice = prefixSegueID + "ConnectingToNameDevice"
        static let appleHealthToFirstLaunchLast = prefixSegue + "AppleHealthToFirstLaunchLast"
        static let deviceChooserToNameDevice = prefixSegue + "DeviceChooserToNameDevice"
    }
    
    //MARK: -
    
    /// The scene identifiers used programatically in the application.
    struct SceneIdentifiers
    {
        static let firstLaunchFirst = prefixSceneID + "FirstLaunchFirst"
    }
}

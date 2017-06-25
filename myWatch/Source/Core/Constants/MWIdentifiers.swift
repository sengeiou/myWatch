//
//  MWIdentifiers.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 09..
//  Copyright © 2017. theMatys. All rights reserved.
//

import Foundation

/// Holds all the identifiers in the application that are used programatically.
struct MWIdentifiers
{
    //MARK: Prefixes
    
    /// The prefix used for all segue identifiers.
    private static let prefixSegueID = "MWSID"
    
    /// The prefix used for all scene identifiers.
    private static let prefixSceneID = "MWSCID"
    
    /// The prefix used for all table view cell identifiers.
    private static let prefixCellID = "MWCID"
    
    //MARK: -
    
    /// The table view cell identifiers used programatically in the application.
    struct CellIdentifiers
    {
        static let deviceChooserDeviceCell = prefixCellID + "DeviceChooserDeviceCell"
    }
    
    //MARK: -
    
    /// The segue identifiers used programatically in the application.
    struct SegueIdentifiers
    {
        static let connectingToNameDevice = prefixSegueID + "ConnectingToNameDevice"
        static let appleHealthToFirstLaunchLast = prefixSegueID + "AppleHealthToFirstLaunchLast"
    }
    
    //MARK: -
    
    /// The scene identifiers used programatically in the application.
    struct SceneIdentifiers
    {
        static let firstLaunchFirst = prefixSceneID + "FirstLaunchFirst"
    }
}

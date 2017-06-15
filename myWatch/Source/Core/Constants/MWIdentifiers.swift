//
//  MWIdentifiers.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 09..
//  Copyright © 2017. theMatys. All rights reserved.
//

import Foundation

struct MWIdentifiers
{
    private static let prefixSegueID = "MWSID"
    private static let prefixSceneID = "MWSCID"
    private static let prefixCellID = "MWCID"
    
    struct CellIdentifiers
    {
        static let deviceChooserDeviceCell = prefixCellID + "DeviceChooserDeviceCell"
    }
    
    struct SegueIdentifiers
    {
        static let firstLaunchFirstToLanguage = prefixSegueID + "FirstLaunchFirstToLanguage"
        static let languageToDeviceChooser = prefixSegueID + "LanguageToDeviceChooser"
        static let deviceChooserToConnecting = prefixSegueID + "DeviceChooserToConnecting"
        static let connectingToNameDevice = prefixSegueID + "ConnectingToNameDevice"
        static let nameDeviceToFirstLaunchLast = prefixSegueID + "NameDeviceToFirstLaunchLast"
        static let firstLaunchLastToMain = prefixSegueID + "FirstLaunchLastToMain"
    }
    
    struct SceneIdentifiers
    {
        static let firstLaunchFirst = prefixSceneID + "FirstLaunchFirst"
        static let language = prefixSceneID + "Language"
        static let deviceChooser = prefixSceneID + "DeviceChooser"
        static let connecting = prefixSceneID + "Connecting"
        static let nameDevice = prefixSceneID + "NameDevice"
        static let firstLaunchLast = prefixSceneID + "FirstLaunchLast"
        static let main = prefixSceneID + "Main"
    }
}

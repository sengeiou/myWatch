//
//  MWConstants.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 10..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

//Struct for holding constants used by the application. (Such as: default colors, image instances, etc.)
struct MWConstants
{
    //MARK: Prefixes
    private static let PREFIX_APPLICATION = "MW"
    private static let PREFIX_ASSET = PREFIX_APPLICATION + "A"
    private static let PREFIX_CELL_IDENTIFIER = PREFIX_APPLICATION + "CID"
    private static let PREFIX_STORYBOARD_IDENTIFIER = PREFIX_APPLICATION + "SID"
    
    //MARK: Names
    static let NAME_APPLICATION: String = "myWatch"
    
    //MARK: Colors
    static let COLOR_TINT: UIColor = UIColor(red: 0.0, green: 1.0, blue: 0.7137, alpha: 1.0)
    static let COLOR_BLUEISH_TINT: UIColor = UIColor(red: 0.0, green: 0.7765, blue: 0.7137, alpha: 1.0)
    static let COLOR_NAVIGATION_BAR: UIColor = UIColor(red: 0.0941, green: 0.0941, blue: 0.0941, alpha: 1.0)
    static let COLOR_CONTEXT_BACKGROUND: UIColor = UIColor(red: 0.0509, green: 0.0509, blue: 0.0509, alpha: 1.0)
    static let COLOR_LIGHT_TEXT_FIELD = UIColor(red: 0.121, green: 0.121, blue: 0.121, alpha: 1.0)
    
    //MARK: Gradients
    static let GRADIENT_TINT: MWGradient = MWGradient(colors: COLOR_TINT, COLOR_BLUEISH_TINT)
    
    //MARK: Bluetooth defaults
    static let BLUETOOTH_DEFAULT_DEVICE_NAME: String = "E-Band"
    
    //MARK: Image animation defaults
    static let IMAGE_ANIMATION_DEFAULT_FPS: Double = 60.0
    
    //MARK: Animatable image names
    static let ANIMATABLE_IMAGE_NAME_CONNECT = PREFIX_ASSET + "UtilConnect-"
    
    //MARK: URLs
    static let URL_SAVE_LOCATION = FileManager().urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("myWatch", isDirectory: true)
    static let URL_MYWATCH_SETTINGS = URL_SAVE_LOCATION.appendingPathComponent("myWatchSettings")
    
    //MARK: IDs - Cell identifiers
    static let CELL_IDENTIFIER_DEVICE_CELL = PREFIX_CELL_IDENTIFIER + "DeviceCell"
    
    //MARK: IDs - Storyboard identifiers - Segue identifiers
    static let STORYBOARD_SEGUE_ID_LANGUAGE_TO_DEVICE_CHOOSER = PREFIX_STORYBOARD_IDENTIFIER + "LanguageToDeviceChooser"
    static let STORYBOARD_SEGUE_ID_DEVICE_CHOOSER_TO_CONNECTING = PREFIX_STORYBOARD_IDENTIFIER + "DeviceChooserToConnecting"
    static let STORYBOARD_SEGUE_ID_CONNECTING_TO_MAIN = PREFIX_STORYBOARD_IDENTIFIER + "ConnectingToMain"
    static let STORYBOARD_SEGUE_ID_CONNECTING_TO_NAME_DEVICE = PREFIX_STORYBOARD_IDENTIFIER + "ConnectingToNameDevice"
    static let STORYBOARD_SEGUE_ID_NAME_DEVICE_TO_MAIN = PREFIX_STORYBOARD_IDENTIFIER + "NameDeviceToMain"
    
    //MARK: IDs - Storyboard identifiers - Scene identifiers
    static let STORYBOARD_SCENE_ID_LANGUAGE = PREFIX_STORYBOARD_IDENTIFIER + "Language"
    static let STORYBOARD_SCENE_ID_DEVICE_CHOOSER = PREFIX_STORYBOARD_IDENTIFIER + "DeviceChooser"
    static let STORYBOARD_SCENE_ID_CONNECTING = PREFIX_STORYBOARD_IDENTIFIER + "Connecting"
    static let STORYBOARD_SCENE_ID_NAME_DEVICE = PREFIX_STORYBOARD_IDENTIFIER + "NameDevice"
    static let STORYBOARD_SCENE_ID_MAIN = PREFIX_STORYBOARD_IDENTIFIER + "Main"
    
    //MARK: Localized texts
    static let LOCALIZED_TEXT_LABEL_CONNECTING_TO_DEVICE = "OKY-i8-c0Z.text.connecting"
    static let LOCALIZED_TEXT_LABEL_SEARCHING_PAIRED_DEVICE = "OKY-i8-c0Z.text.searching"
    
    //MARK: Localized texts - Strings filenames
    static let LOCALIZED_STRINGS_FILE_DYNAMIC_STRINGS = "DynamicStrings"
}

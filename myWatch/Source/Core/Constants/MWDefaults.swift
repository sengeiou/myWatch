//
//  File.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 09..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

/// Holds all the defaults that the application uses.
struct MWDefaults
{
    /// Application-related defaults.
    struct Application
    {
        static let applicationName = "myWatch"
        static let applicationPrefix = "MW"
    }
    
    //MARK: -
    
    /// Bluetooth-related defaults.
    struct Bluetooth
    {
        static let defaultDeviceName = "E-Band"
    }
    
    //MARK: -
    
    /// Default colors.
    struct Colors
    {
        static let defaultTintColor: UIColor = UIColor(red: 0.0, green: 1.0, blue: 0.7137, alpha: 1.0)
        static let alternativeTintColor: UIColor = UIColor(red: 0.0, green: 0.7765, blue: 0.7137, alpha: 1.0)
        static let defaultNavigationBarColor: UIColor = UIColor(red: 0.0941, green: 0.0941, blue: 0.0941, alpha: 1.0)
        static let defaultBackgroundColor: UIColor = UIColor(red: 0.0509, green: 0.0509, blue: 0.0509, alpha: 1.0)
        static let defaultTextFieldColor = UIColor(red: 0.121, green: 0.121, blue: 0.121, alpha: 1.0)
    }
    
    //MARK: -
    
    /// Default gradients.
    struct Gradients
    {
        static let defaultGradient: MWGradient = MWGradient(colors: Colors.defaultTintColor, Colors.alternativeTintColor)
    }
    
    //MARK: -
    
    /// Animation-related defaults.
    struct Animation
    {
        static let defaultFramesPerSecond: Double = 60.0
    }
}

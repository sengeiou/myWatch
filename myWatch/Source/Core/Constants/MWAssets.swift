//
//  MWAssets.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 09..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

fileprivate let prefixAsset: String = "MWAsset"

/// Holds all the assets that are instantiated programatically by the application.
struct MWAssets
{
    //MARK: Prefixes
    
    /// The prefix all asset names use.
    private static let _prefixAsset = "MWA"
    
    //MARK: -
    
    /// Holds all the image assets that are instantiated programatically by the application.
    struct Images
    {
        static let imageNoImage = MWImageAsset(name: prefixAsset + "NoImage")
        
        static let imageFirstLaunchConnect = MWImageAsset(name: _prefixAsset + "FirstLaunchConnect")
        static let imageFirstLaunchDeviceChooser = MWImageAsset(name: _prefixAsset + "FirstLaunchDeviceChooser")
        static let imageFirstLaunchExport = MWImageAsset(name: _prefixAsset + "FirstLaunchExport")
        static let imageFirstLaunchNameDevice = MWImageAsset(name: _prefixAsset + "FirstLaunchNameDevice")
        static let imageFirstLaunchLanguage = MWImageAsset(name: _prefixAsset + "FirstLaunchLanguage")
        
        static let imageGeneralBluetooth = MWImageAsset(name: prefixAsset + "GeneralBluetooth")
    }
}

//MARK: -

/// Represents an image asset.
///
/// The main focus of this small class is on `getImage(inBundle:traits:)`
class MWImageAsset
{
    //MARK: Instance variables
    
    /// The name of the image asset.
    var name: String
    
    /// The image this class represents.
    var image: UIImage?
    
    //MARK: - Initializers
    
    /// Makes an `MWImageAsset` instance out of the given parameters.
    ///
    /// - Parameter name: The name of the image this asset represents.
    init(name: String)
    {
        //Store the name and make the image out of the name
        self.name = name
        self.image = UIImage(named: name)
    }
    
    //MARK: Instance functions
    
    /// When instantiating an image programatically, it must be instantiated using initializer `UIImage.init(named:in:compatibleWith)`, else it will not load an image and returns nil.
    ///
    /// This function was meant to be used to get the image that this class represents.
    ///
    /// - Parameters:
    ///   - bundle: The bundle the image is located in.
    ///   - traits: The traits this image is compatible with.
    /// - Returns: An optional which is supposed to contain the image this class represents.
    func getImage(in bundle: Bundle?, traits: UITraitCollection?) -> UIImage?
    {
        return UIImage(named: name, in: bundle, compatibleWith: traits)
    }
}

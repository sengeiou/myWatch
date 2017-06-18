//
//  MWAssets.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 09..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

struct MWAssets
{
    private static let prefixAsset = "MWA"
    
    struct Images
    {
        //MARK: Images
        static let imageNoImage = MWImageAsset(name: prefixAsset + "NoImage")
        
        static let imageFirstLaunchConnect = MWImageAsset(name: prefixAsset + "FirstLaunchConnect")
        static let imageFirstLaunchDeviceChooser = MWImageAsset(name: prefixAsset + "FirstLaunchDeviceChooser")
        static let imageFirstLaunchExport = MWImageAsset(name: prefixAsset + "FirstLaunchExport")
        static let imageFirstLaunchNameDevice = MWImageAsset(name: prefixAsset + "FirstLaunchNameDevice")
        static let imageFirstLaunchLanguage = MWImageAsset(name: prefixAsset + "FirstLaunchLanguage")
    }
}

class MWImageAsset
{
    private var name: String
    private var image: UIImage?
    
    init(name: String)
    {
        self.name = name
        self.image = UIImage(named: name)
    }
    

    func getName() -> String
    {
        return self.name
    }

    func getImage() -> UIImage?
    {
        return self.image
    }
    
    func getImage(inBundle bundle: Bundle?, traits: UITraitCollection?) -> UIImage?
    {
        return UIImage(named: name, in: bundle, compatibleWith: traits)
    }
}

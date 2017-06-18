//
//  MWImageView.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 23..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

@IBDesignable
class MWImageView: UIImageView
{
    @IBInspectable var tintingColor: UIColor = MWDefaults.Colors.defaultTintColor
    {
        didSet
        {
            if(autoUpdate)
            {
                imageHasAlreadyBeenTinted = true
                self.image = tintedImage()
            }
        }
    }
    
    @IBInspectable var gradientTinted: Bool = false
    {
        didSet
        {
            if(autoUpdate)
            {
                imageHasAlreadyBeenTinted = true
                self.image = tintedImage()
            }
        }
    }
    
    @IBInspectable var tintingGradient: MWGradient = MWDefaults.Gradients.defaultGradient
    {
        didSet
        {
            if(autoUpdate)
            {
                imageHasAlreadyBeenTinted = true
                self.image = tintedImage()
            }
        }
    }
    
    override var image: UIImage?
    {
        didSet
        {
            if(!imageHasAlreadyBeenTinted)
            {
                imageHasAlreadyBeenTinted = true
                self.image = tintedImage()
            }
            else
            {
                imageHasAlreadyBeenTinted = false
            }
        }
    }
    
    var autoUpdate = true
    var imageHasAlreadyBeenTinted: Bool = false
    
    override init(frame: CGRect)
    {
        super.init(frame: frame)
        
        self.image = tintedImage()
    }
    
    override init(image: UIImage?)
    {
        super.init(image: image)
        
        self.image = tintedImage()
    }
    
    override init(image: UIImage?, highlightedImage: UIImage?)
    {
        super.init(image: image, highlightedImage: highlightedImage)
        
        self.image = tintedImage()
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        
        self.image = tintedImage()
    }
    
    private func tintedImage() -> UIImage?
    {
        if(self.gradientTinted)
        {
            return self.image?.tinted(with: self.tintingGradient)
        }
        else
        {
            return self.image?.tinted(with: self.tintingColor)
        }
    }
}

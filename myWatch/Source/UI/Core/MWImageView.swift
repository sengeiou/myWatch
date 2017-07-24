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
            if(!silent)
            {
                self.silently().image = tintedImage()
            }
            else
            {
                silent = false
            }
        }
    }
    
    @IBInspectable var gradientTinted: Bool = false
    {
        didSet
        {
            if(!silent)
            {
                self.silently().image = tintedImage()
            }
            else
            {
                silent = false
            }
        }
    }
    
    @IBInspectable var tintingGradient: MWGradient = MWDefaults.Gradients.defaultGradient
    {
        didSet
        {
            if(!silent)
            {
                self.silently().image = tintedImage()
            }
            else
            {
                silent = false
            }
        }
    }
    
    override var image: UIImage?
    {
        didSet
        {
            if(!silent)
            {
                self.silently().image = tintedImage()
            }
            else
            {
                silent = false
            }
        }
    }
    
    private var silent = false
    
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
    
    func silently() -> MWImageView
    {
        silent = true
        return self
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

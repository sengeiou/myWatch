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
        if(gradientTinted)
        {
            return tintedImage(withGradient: tintingGradient)
        }
        else
        {
            return tintedImage(withColor: tintingColor)
        }
    }
    
    func tintedImage(withColor: UIColor) -> UIImage?
    {
        var ret: UIImage? = nil
        
        MWUtil.execute(ifNotNil: self.image) {
            MWUtil.execute(ifNotNil: self.image!.cgImage, execution: {
                let rect: CGRect = CGRect(x: 0, y: 0, width: self.image!.size.width, height: self.image!.size.height)
                
                UIGraphicsBeginImageContextWithOptions(self.image!.size, false, self.image!.scale)
                let context: CGContext? = UIGraphicsGetCurrentContext()
                
                MWUtil.execute(ifNotNil: context, execution: {
                    context!.translateBy(x: 0, y: self.image!.size.height)
                    context!.scaleBy(x: 1.0, y: -1.0)
                    context!.setBlendMode(.normal)
                    context!.clip(to: rect, mask: self.image!.cgImage!)
                    self.tintingColor.setFill()
                    context!.fill(rect)
                    
                    ret = UIGraphicsGetImageFromCurrentImageContext()
                    
                    UIGraphicsEndImageContext()
                })
            })
        }
        
        return ret
    }
    
    func tintedImage(withGradient: MWGradient) -> UIImage?
    {
        var ret: UIImage? = nil
        
        MWUtil.execute(ifNotNil: self.image) {
            MWUtil.execute(ifNotNil: self.image!.cgImage, execution: {
                let rect: CGRect = CGRect(x: 0, y: 0, width: self.image!.size.width, height: self.image!.size.height)
                
                UIGraphicsBeginImageContextWithOptions(self.image!.size, false, self.image!.scale)
                let context: CGContext? = UIGraphicsGetCurrentContext()
                
                MWUtil.execute(ifNotNil: context, execution: {
                    context!.translateBy(x: 0, y: self.image!.size.height)
                    context!.scaleBy(x: 1.0, y: -1.0)
                    context!.setBlendMode(.normal)
                    context!.clip(to: rect, mask: self.image!.cgImage!)
                    context!.drawLinearGradient(self.tintingGradient.cgGradient(), start: CGPoint(x: 0.0, y: 0.0), end: CGPoint(x: 0, y: rect.height), options: CGGradientDrawingOptions(rawValue: 0))
                    
                    ret = UIGraphicsGetImageFromCurrentImageContext()
                    
                    UIGraphicsEndImageContext()
                })
            })
        }
        
        return ret
    }
}

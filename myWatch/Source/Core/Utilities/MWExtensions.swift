//
//  MWExtensions.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 20..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

extension UIView
{
    func transfer(to view: UIView)
    {
        self.translatesAutoresizingMaskIntoConstraints = true
        view.addSubview(self)
        
        MWUtil.execute(ifNotNil: self.superview, execution: { 
            self.frame = self.superview!.convert(self.frame, to: self.superview!)
        }) { 
            fatalError("Could not transfer view, because the view to transfer to is not the superview of this view.")
        }
    }
    
    func updateFrame()
    {
        MWUtil.execute(ifNotNil: self.superview, execution: { 
            self.translatesAutoresizingMaskIntoConstraints = true
            self.frame = self.superview!.convert(self.frame, from: self.superview!)
        }) { 
            MWLError("Could not update frame of view, because its superview is nil.", module: nil)
        }
    }
}

extension UIColor
{
    func getComponentRed() -> CGFloat
    {
        return self.cgColor.components![0]
    }
    
    func getComponentGreen() -> CGFloat
    {
        return self.cgColor.components![1]
    }
    
    func getComponentBlue() -> CGFloat
    {
        return self.cgColor.components![2]
    }
    
    func getComponentAlpha() -> CGFloat
    {
        return self.cgColor.components![3]
    }
}

class MWImageAnimation
{
    private var repeatCount: Int
    private var duration: TimeInterval
    private var frames: [UIImage]
    
    init(repeatCount: Int, duration: TimeInterval, frames: [UIImage])
    {
        self.repeatCount = repeatCount
        self.duration = duration
        self.frames = frames
    }
    
    fileprivate func attach(to imageView: UIImageView)
    {
        imageView.animationRepeatCount = repeatCount
        imageView.animationDuration = duration
        imageView.animationImages = frames
    }
}

extension UIImageView
{
    func attachImageAnimation(_ imageAnimation: MWImageAnimation)
    {
        imageAnimation.attach(to: self)
    }
    
    func isAnimatable() -> Bool
    {
        return self.animationImages != nil
    }
}

extension UIImage
{
    func tinted(with color: UIColor) -> UIImage?
    {
        var ret: UIImage?
        
        MWUtil.execute(ifNotNil: self.cgImage) {
            let rect: CGRect = CGRect(x: 0, y: 0, width: self.size.width, height: self.size.height)
            
            UIGraphicsBeginImageContextWithOptions(self.size, false, self.scale)
            let context: CGContext? = UIGraphicsGetCurrentContext()
            
            MWUtil.execute(ifNotNil: context, execution: {
                context!.translateBy(x: 0, y: self.size.height)
                context!.scaleBy(x: 1.0, y: -1.0)
                context!.setBlendMode(.normal)
                context!.clip(to: rect, mask: self.cgImage!)
                color.setFill()
                context!.fill(rect)
                
                ret = UIGraphicsGetImageFromCurrentImageContext()
                
                UIGraphicsEndImageContext()
            })
        }
        
        return ret
    }
    
    func tinted(with gradient: MWGradient) -> UIImage?
    {
        var ret: UIImage?
        
        MWUtil.execute(ifNotNil: self.cgImage) {
            let rect: CGRect = CGRect(x: 0, y: 0, width: self.size.width, height: self.size.height)
            
            UIGraphicsBeginImageContextWithOptions(self.size, false, self.scale)
            let context: CGContext? = UIGraphicsGetCurrentContext()
            
            MWUtil.execute(ifNotNil: context, execution: {
                context!.translateBy(x: 0, y: self.size.height)
                context!.scaleBy(x: 1.0, y: -1.0)
                context!.setBlendMode(.normal)
                context!.clip(to: rect, mask: self.cgImage!)
                context!.drawLinearGradient(gradient.cgGradient(), start: CGPoint(x: 0.0, y: 0.0), end: CGPoint(x: 0, y: rect.height), options: CGGradientDrawingOptions(rawValue: 0))
                
                ret = UIGraphicsGetImageFromCurrentImageContext()
                
                UIGraphicsEndImageContext()
            })
        }
        
        return ret
    }
}

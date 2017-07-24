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
    /// Transfers the view into another view.
    ///
    /// This involves adding this view as a subview of `view` and converting this view's frame into the new superview's coordinate system.
    ///
    /// - Parameter view: The view this view should be transferred to.
    func transfer(to view: UIView)
    {
        view.addSubview(self)
        self.updateFrame()
    }
    
    /// Updates the view's frame to its superview's coordinate system.
    func updateFrame()
    {
        self.superview ??! {
            self.translatesAutoresizingMaskIntoConstraints = true
            self.frame = self.superview!.convert(self.frame, from: self.superview!)
        } >< {
            MWLError("Could not update frame of view, because its superview is nil.", module: nil)
        }
    }
}

//MARK: -

extension UIColor
{
    /// Used to retrieve the red component from this `UIColor`.
    ///
    /// - Returns: The red component of this `UIColor`
    func getComponentRed() -> CGFloat
    {
        return self.cgColor.components![0]
    }
    
    /// Used to retrieve the green component from this `UIColor`.
    ///
    /// - Returns: The green component of this `UIColor`
    func getComponentGreen() -> CGFloat
    {
        return self.cgColor.components![1]
    }
    
    /// Used to retrieve the blue component from this `UIColor`.
    ///
    /// - Returns: The blue component of this `UIColor`
    func getComponentBlue() -> CGFloat
    {
        return self.cgColor.components![2]
    }
    
    /// Used to retrieve the alpha component from this `UIColor`.
    ///
    /// - Returns: The alpha component of this `UIColor`
    func getComponentAlpha() -> CGFloat
    {
        return self.cgColor.components![3]
    }
    
    /// Returns this `UIColor`, but with `amount` added to each component (except the alpha).
    ///
    /// - Parameter amount: The scalar that should be added to each component.
    /// - Returns: A new `UIColor` with `amount` added to each of its components (except the alpha).
    func adding(_ amount: CGFloat) -> UIColor
    {
        return UIColor(red: self.getComponentRed() + amount, green: self.getComponentGreen() + amount, blue: self.getComponentBlue() + amount, alpha: self.getComponentAlpha())
    }
    
    /// Returns this `UIColor`, but with `amount` substracted from each component (except the alpha).
    ///
    /// - Parameter amount: The scalar that should be substracted from each component.
    /// - Returns: A new `UIColor` with `amount` substracted from each of its components (except the alpha).
    func substracting(_ amount: CGFloat) -> UIColor
    {
        return UIColor(red: self.getComponentRed() - amount, green: self.getComponentGreen() - amount, blue: self.getComponentBlue() - amount, alpha: self.getComponentAlpha())
    }
    
    /// Returns this `UIColor`, but with `amount` multiplied by each component (except the alpha).
    ///
    /// - Parameter amount: The scalar that should be multiplied by each component.
    /// - Returns: A new `UIColor` with `amount` multiplied by each of its components (except the alpha).
    func multiplying(_ amount: CGFloat) -> UIColor
    {
        return UIColor(red: self.getComponentRed() * amount, green: self.getComponentGreen() * amount, blue: self.getComponentBlue() * amount, alpha: self.getComponentAlpha())
    }
}

//MARK: -

extension UIImageView
{
    /// Attaches an image animation object to this image view.
    ///
    /// - Parameter imageAnimation: The image animation that should be attached to this image view.
    func attachImageAnimation(_ imageAnimation: MWImageAnimation)
    {
        self.animationRepeatCount = imageAnimation.repeatCount
        self.animationDuration = imageAnimation.duration
        self.animationImages = imageAnimation.frames
    }
    
    /// Used to determine whether this image view is animatable.
    ///
    /// - Returns: A boolean which indicates whether this image view is animatable.
    func isAnimatable() -> Bool
    {
        return self.animationImages != nil
    }
}

//MARK: -

extension UIImage
{
    /// Tints the image with a single color.
    ///
    /// - Parameter color: The color the image should be tinted with.
    /// - Returns: The tinted image.
    func tinted(with color: UIColor) -> UIImage?
    {
        //Declare a default return value
        var ret: UIImage?
        
        //Check if the image can be used
        self.cgImage ?! {
            //Create a rectandle which is capable of holding the new image
            let rect: CGRect = CGRect(x: 0, y: 0, width: self.size.width, height: self.size.height)
            
            //Create the context
            UIGraphicsBeginImageContextWithOptions(self.size, false, self.scale)
            let context: CGContext? = UIGraphicsGetCurrentContext()
            
            //Check if the context is valid
            context ?! {
                //If yes, make the image
                context!.translateBy(x: 0, y: self.size.height)
                context!.scaleBy(x: 1.0, y: -1.0)
                context!.setBlendMode(.normal)
                context!.clip(to: rect, mask: self.cgImage!)
                color.setFill()
                context!.fill(rect)
                
                //Get the new image and end the context
                ret = UIGraphicsGetImageFromCurrentImageContext()
                
                UIGraphicsEndImageContext()
            }
        }
        
        return ret
    }
    
    /// Tints the image with a gradient.
    ///
    /// - Parameter gradient: The gradient the image should be tinted with.
    /// - Returns: The tinted image.
    func tinted(with gradient: MWGradient) -> UIImage?
    {
        //Declare a default return value
        var ret: UIImage?
        
        //Check if the image can be used
        self.cgImage ?! {
            //Create a rectandle which is capable of holding the new image
            let rect: CGRect = CGRect(x: 0, y: 0, width: self.size.width, height: self.size.height)
            
            //Create the context
            UIGraphicsBeginImageContextWithOptions(self.size, false, self.scale)
            let context: CGContext? = UIGraphicsGetCurrentContext()
            
            //Check if the context is valid
            context ?! {
                //If yes, make the image
                context!.translateBy(x: 0, y: self.size.height)
                context!.scaleBy(x: 1.0, y: -1.0)
                context!.setBlendMode(.normal)
                context!.clip(to: rect, mask: self.cgImage!)
                context!.drawLinearGradient(gradient.cgGradient(), start: CGPoint(x: 0.0, y: 0.0), end: CGPoint(x: 0, y: rect.height), options: CGGradientDrawingOptions(rawValue: 0))
                
                //Get the new image and end the context
                ret = UIGraphicsGetImageFromCurrentImageContext()
                
                UIGraphicsEndImageContext()
            }
        }
        
        return ret
    }
}

//MARK: -

extension CGSize
{
    /// Checks if the current `CGSize` is greater than the one specified in `other` by any means.
    ///
    /// - Parameter other: The `CGSize` the current one should be compared with.
    /// - Returns: The boolean indicating whether the current `CGSize` is greater than `other`.
    func greater(than other: CGSize) -> Bool
    {
        if(self.width > other.width || self.height > other.height || self.width > other.height || self.height > other.width)
        {
            return true
        }
        else
        {
            return false
        }
    }
    
    /// Checks if the current `CGSize` is less than the one specified in `other` by any means.
    ///
    /// - Parameter other: The `CGSize` the current one should be compared with.
    /// - Returns: The boolean indicating whether the current `CGSize` is less than `other`.
    func less(than other: CGSize) -> Bool
    {
        if(self.width < other.width || self.height < other.height || self.width < other.height || self.height < other.width)
        {
            return true
        }
        else
        {
            return false
        }
    }
    
    /// Checks if the current `CGSize` equals to the one specified in `other`
    ///
    /// - Parameter other: The `CGSize` the current one should be compared with.
    /// - Returns: The boolean indicating whether the current `CGSize` equals to `other`.
    func equals(to other: CGSize) -> Bool
    {
        if(self.width == other.width || self.height == other.height)
        {
            return true
        }
        else
        {
            return false
        }
    }
}

//MARK: -

extension UIButton
{
    /// Disables the button.
    func disableButton()
    {
        self.isEnabled = false
    }
    
    /// Enables the button.
    func enableButton()
    {
        self.isEnabled = true
    }
    
    /// Selects the button.
    func selectButton()
    {
        self.isSelected = true
    }
    
    /// Deselects the button.
    func deselectButton()
    {
        self.isSelected = false
    }
    
    /// Toggles the enable on the button.
    func toggleEnabled()
    {
        self.isEnabled = self.isEnabled ? false : true
    }
    
    /// Toggles the selection of the button.
    func toggleSelected()
    {
        self.isSelected = self.isSelected ? false : true
    }
}

//MARK: -

extension CGRect
{
    /// Scales the rectangle by the given values.
    ///
    /// - Parameters:
    ///   - width: The width that should be added to the current width.
    ///   - height: The height that should be added to the current height.
    /// - Returns: The transformed rectangle.
    func scaleBy(width: CGFloat, height: CGFloat) -> CGRect
    {
        return CGRect(x: self.origin.x, y: self.origin.y, width: self.width + width, height: self.height + height)
    }
    
    /// Scales the rectangle by the given values while maintaining its center `x` and `y` position.
    ///
    /// - Parameters:
    ///   - width: The width that should be added to the current width.
    ///   - height: The height that should be added to the current height.
    /// - Returns: The transformed rectangle.
    func scaleByCentered(width: CGFloat, height: CGFloat) -> CGRect
    {
        return CGRect(x: self.origin.x - (width / 2), y: self.origin.y - (height / 2), width: self.width + width, height: self.height + height)
    }
    
    /// Transforms the rectangle to the given position while maintaining its width and the height
    ///
    /// - Parameter point: The point this rectangle's origin should be put to.
    /// - Returns: The transfromed rectangle.
    func withPosition(_ point: CGPoint) -> CGRect
    {
        return withPosition(x: point.x, y: point.y)
    }
    
    /// Transforms the rectangle to the given position while maintaning its width and the height.
    ///
    /// - Parameters:
    ///   - x: The `x` value of the point that this rectangle's origin should be put to.
    ///   - y: The `y` value of the point that this rectangle's origin should be put to.
    /// - Returns: The transformed rectangle.
    func withPosition(x: CGFloat, y: CGFloat) -> CGRect
    {
        return CGRect(x: x, y: y, width: self.width, height: self.height)
    }
    
    /// Scales the rectangle to the given size by maintaining its `x` and `y` position.
    ///
    /// - Parameter size: The size this rectangle should be scaled to.
    /// - Returns: The transformed rectangle.
    func withSize(_ size: CGSize) -> CGRect
    {
        return withSize(width: size.width, height: size.height)
    }
    
    /// cales the rectangle to the given size by maintaining its `x` and `y` position.
    ///
    /// - Parameters:
    ///   - width: The width of the size that this rectangle should be scaled to.
    ///   - height: The height of the size that this rectangle should be scaled to.
    /// - Returns: The transformed rectangle.
    func withSize(width: CGFloat, height: CGFloat) -> CGRect
    {
        return CGRect(x: self.origin.x, y: self.origin.y, width: width, height: height)
    }
}

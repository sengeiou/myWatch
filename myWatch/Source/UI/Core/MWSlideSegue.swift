//
//  MWSlideSegue.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 22.
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWSlideSegue: UIStoryboardSegue
{
    internal var animation: MWSlideSegueAnimation!
    
    override init(identifier: String?, source: UIViewController, destination: UIViewController)
    {
        super.init(identifier: identifier, source: source, destination: destination)
        animation = MWSlideSegueAnimation(identifier: identifier != nil ? identifier! : "<MISSING SEGUE IDENTIFIER>", sourceViewController: source, destinationViewController: destination)
    }
    
    override func perform()
    {
        animation.prepare()
        animation.perform()
    }
}

class MWSlideSegue_DNMIV: MWSlideSegue
{
    override init(identifier: String?, source: UIViewController, destination: UIViewController)
    {
        super.init(identifier: identifier, source: source, destination: destination)
    }
    
    override func perform()
    {
        self.animation = self.animation.doesNotMoveImageView()
        super.perform()
    }
}

class MWSlideSegue_ADIV_WB: MWSlideSegue
{
    override init(identifier: String?, source: UIViewController, destination: UIViewController)
    {
        super.init(identifier: identifier, source: source, destination: destination)
    }
    
    override func perform()
    {
        self.animation = self.animation.animatesDestinationImageView().delaysButton()
        super.perform()
    }
}

class MWSlideSegueAnimation
{
    private var identifier: String
    
    private var sourceViewController: UIViewController
    private var destinationViewController: UIViewController
    
    private var sourceView: UIView
    private var destinationView: UIView
    
    private var outerAppSource: MWOuterAppViewController?
    private var outerAppDestination: MWOuterAppViewController?
    
    private var destinationBackground: UIColor!
    
    private var sourceImageView: MWTintedImageView?
    private var destinationImageView: MWTintedImageView?
    
    private var destinationButton: MWButton?
    
    private var destinationImageAnimation: MWImageAnimation?
    
    init(identifier: String, sourceViewController: UIViewController, destinationViewController: UIViewController)
    {
        self.identifier = identifier
        
        self.sourceViewController = sourceViewController
        self.destinationViewController = destinationViewController
        
        self.sourceView = sourceViewController.view
        self.destinationView = destinationViewController.view
        
        self.destinationBackground = destinationView.backgroundColor!
    }
    
    func doesNotMoveImageView() -> MWSlideSegueAnimation
    {
        MWUtil.execute(ifNil: outerAppSource) { 
            MWUtil.downcast(to: &self.outerAppSource, from: self.sourceViewController)
        }
        
        MWUtil.execute(ifNil: outerAppDestination) { 
            MWUtil.downcast(to: &self.outerAppDestination, from: self.destinationViewController)
        }
        
        sourceImageView = outerAppSource!.getImageView()
        destinationImageView = outerAppDestination!.getImageView()
        
        return self
    }
    
    func animatesDestinationImageView() -> MWSlideSegueAnimation
    {
        MWUtil.execute(ifNil: destinationImageAnimation) {
            let _: MWSlideSegueAnimation = self.doesNotMoveImageView()
            
            self.destinationImageAnimation = self.outerAppDestination!.getImageAnimation()
        }
        
        return self
    }
    
    func delaysButton() -> MWSlideSegueAnimation
    {
        MWUtil.execute(ifNil: destinationButton) { 
            MWUtil.downcast(to: &self.outerAppDestination, from: self.destinationViewController)
        }
        
        destinationButton = outerAppDestination!.getButton()
        
        return self
    }
    
    func prepare()
    {
        let window = UIApplication.shared.keyWindow!

        //Prepare the base animation
        window.backgroundColor = sourceView.backgroundColor
        sourceView.backgroundColor = UIColor.clear
        destinationView.backgroundColor = UIColor.clear
        
        MWUtil.execute(ifNotNil: sourceImageView) { 
            MWUtil.execute(ifNotNil: self.destinationImageView, execution: {
                let copied: MWTintedImageView = MWTintedImageView(image: self.destinationImageView!.image)
                
                copied.setGradientTinted(self.destinationImageView!.isGradientTinted())
                copied.translatesAutoresizingMaskIntoConstraints = true
                copied.frame = self.destinationImageView!.superview!.convert(self.destinationImageView!.frame, to: window)
                
                self.sourceImageView!.alpha = 0.0
                self.destinationImageView!.alpha = 0.0
                
                window.insertSubview(copied, aboveSubview: self.destinationView)
                
                MWUtil.execute(ifNotNil: self.destinationImageAnimation, execution: { 
                    copied.attachImageAnimation(self.destinationImageAnimation!)
                })
                
                self.destinationImageView = copied
            })
        }
        
        destinationView.frame = destinationView.frame.offsetBy(dx: window.frame.width / 3, dy: 0)
        window.insertSubview(destinationView, aboveSubview: sourceView)
        destinationView.alpha = 0.0
        
        /*//If specified, do the preparation to make the image view not move
        MWUtil.execute(ifNotNil: sourceImageView) { 
            MWUtil.execute(ifNotNil: self.destinationImageView, execution: {
                self.sourceImageView!.image = self.destinationImageView!.image
                
                self.destinationImageView!.translatesAutoresizingMaskIntoConstraints = true
                self.destinationImageView!.frame = self.destinationImageView!.frame.offsetBy(dx: -(window.frame.width / 3), dy: 0)
                
                self.destinationImageView!.alpha = 1.0
            })
        }
        
        //If specified, do the preparation to make the button delayed.
        MWUtil.execute(ifNotNil: destinationButton) { 
            self.destinationButton!.alpha = 0.0
        }
        
        MWUtil.execute(ifNotNil: self.destinationImageAnimation) {
            self.sourceImageView!.image = self.destinationImageView!.image
            self.sourceImageView!.attachImageAnimation(self.destinationImageAnimation!)
        }*/
    }
    
    func perform()
    {
        let window = UIApplication.shared.keyWindow!
        
        UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseInOut, animations: {
            //Execute the base animation
            window.backgroundColor = self.destinationBackground!
            
            self.sourceView.frame = self.sourceView.frame.offsetBy(dx: -(window.frame.width / 3), dy: 0)
            self.sourceView.alpha = 0.0
            
            self.destinationView.frame = self.destinationView.frame.offsetBy(dx: -(window.frame.width / 3), dy: 0)
            self.destinationView.alpha = 1.0
            
            MWUtil.execute(ifNotNil: self.sourceImageView, execution: { 
                MWUtil.execute(ifNotNil: self.destinationImageView, execution: { 
                    self.sourceImageView!.frame = self.sourceImageView!.frame.offsetBy(dx: window.frame.width / 3, dy: 0)
                })
            })
            
            /*
            //If specified, make the button hidden for the delayed animation
            MWUtil.execute(ifNotNil: self.destinationButton, execution: { 
                self.destinationButton!.alpha = 0.0
            })*/
        }, completion: { (finished: Bool) in
            /*MWUtil.execute(ifNotNil: self.destinationButton, execution: {
                UIView.animate(withDuration: 0.15, delay: 0.0, options: .curveEaseOut, animations: {
                    self.destinationButton!.alpha = 1.0
                }, completion: nil)
            })*/
            
            MWUtil.execute(ifNotNil: self.destinationImageAnimation, execution: { 
                if(self.sourceImageView!.animationDuration <= 0.35)
                {
                    self.sourceImageView!.stopAnimating()
                    
                    self.destinationView.backgroundColor = self.destinationBackground
                    self.sourceViewController.present(self.destinationViewController, animated: false, completion: nil)
                }
            }, elseExecution: { 
                self.destinationView.backgroundColor = self.destinationBackground
                self.sourceViewController.present(self.destinationViewController, animated: false, completion: nil)
            })
        })
        
        MWUtil.execute(ifNotNil: destinationImageAnimation) { 
            self.destinationImageView!.startAnimating()
            
            if(self.destinationImageView!.animationDuration > 0.35)
            {
                DispatchQueue.main.asyncAfter(deadline: .now() + self.destinationImageView!.animationDuration, execute: {
                    self.destinationImageView!.stopAnimating()
                    
                    self.destinationView.backgroundColor = self.destinationBackground
                    self.sourceViewController.present(self.destinationViewController, animated: false, completion: nil)
                })
            }
        }
    }
}

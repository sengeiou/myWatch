//
//  MWFirstLaunchAnimatedTransitioning.swift
//  myWatch
//
//  Created by Máté on 2017. 07. 08..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWFirstLaunchAnimatedTransitioning: NSObject, UIViewControllerAnimatedTransitioning
{
    var reversed: Bool = false
    var explicitControl: ((UIViewControllerContextTransitioning) -> ())?
    
    private var source: UIViewController!
    private var destination: UIViewController!
    
    private var sourceView: UIView!
    private var destinationView: UIView!
    
    private var containerView: UIView!
    
    private var sourceBackground: UIColor?
    private var destinationBackground: UIColor?
    
    func transitionDuration(using transitionContext: UIViewControllerContextTransitioning?) -> TimeInterval
    {
        return 0.35
    }
    
    func animateTransition(using transitionContext: UIViewControllerContextTransitioning)
    {
        source = transitionContext.viewController(forKey: .from)!
        destination = transitionContext.viewController(forKey: .to)!
        
        sourceView = source.view
        destinationView = destination.view
        
        containerView = transitionContext.containerView
        
        //Save the background of the source and destination view, so we can animate the backgrounds later
        sourceBackground = sourceView.backgroundColor
        destinationBackground = destinationView.backgroundColor
        
        //Prepare the backgrounds
        containerView.backgroundColor = sourceBackground
        sourceView.backgroundColor = UIColor.clear
        destinationView.backgroundColor = UIColor.clear
        
        //Prepare the destination view
        destinationView.alpha = 0.0
        destinationView.frame = sourceView.frame
        destinationView.frame = destinationView.frame.offsetBy(dx: reversed ? -(containerView.frame.width / 2) : containerView.frame.width / 2, dy: 0.0)
        
        //Insert the destination view to the view hiearchy above the source view
        containerView.insertSubview(destinationView, aboveSubview: sourceView)
        
        //Execute any further operations explicitly, if explicit control is specified
        explicitControl ??! {
            self.explicitControl!(transitionContext)
        } >< {
            self.executeAnimation(using: transitionContext)
        }
    }
    
    func executeAnimation(using transitionContext: UIViewControllerContextTransitioning)
    {
        /*//Animate the transition
        UIView.animate(withDuration: transitionDuration(using: transitionContext), delay: 0.0, options: [.curveEaseInOut, .preferredFramesPerSecond60], animations: {
            //Animate to the destination's saved background
            self.containerView.backgroundColor = self.destinationBackground
            
            //Shift the frame of the source view and fade it out
            self.sourceView.frame = self.sourceView.frame.offsetBy(dx: self.reversed ? self.containerView.frame.width / 2 : -(self.containerView.frame.width / 2), dy: 0.0)
            self.sourceView.alpha = 0.0
            
            //Shift the frame of the destination view and fade it in
            self.destinationView.frame = self.destinationView.frame.offsetBy(dx: self.reversed ? self.containerView.frame.width / 2 : -(self.containerView.frame.width / 2), dy: 0.0)
            self.destinationView.alpha = 1.0
        }) { (finished: Bool) in
            //Remove the source view from the hierarchy
            self.sourceView.removeFromSuperview()
            
            //Restore the source view's frame and background color for potential reverse animation
            self.sourceView.frame = self.sourceView.frame.withPosition(x: 0.0, y: 0.0)
            self.sourceView.backgroundColor = self.sourceBackground
            
            //Restore the backgrounds
            self.destinationView.backgroundColor = self.destinationBackground
            self.containerView.backgroundColor = UIColor.black
            
            //Complete the transition
            self.destination.viewController(self.destination, didGetPresentedBy: self.source)
            transitionContext.completeTransition(!transitionContext.transitionWasCancelled)
        }*/
        
        UIView.animate(withDuration: transitionDuration(using: transitionContext), delay: 0.0, usingSpringWithDamping: 1.0, initialSpringVelocity: 0.0, options: .preferredFramesPerSecond60, animations: { 
            //Animate to the destination's saved background
            self.containerView.backgroundColor = self.destinationBackground
            
            //Shift the frame of the source view and fade it out
            self.sourceView.frame = self.sourceView.frame.offsetBy(dx: self.reversed ? self.containerView.frame.width / 2 : -(self.containerView.frame.width / 2), dy: 0.0)
            self.sourceView.alpha = 0.0
            
            //Shift the frame of the destination view and fade it in
            self.destinationView.frame = self.destinationView.frame.offsetBy(dx: self.reversed ? self.containerView.frame.width / 2 : -(self.containerView.frame.width / 2), dy: 0.0)
            self.destinationView.alpha = 1.0
        }) { (finished: Bool) in
            //Remove the source view from the hierarchy
            self.sourceView.removeFromSuperview()
            
            //Restore the source view's frame and background color for potential reverse animation
            self.sourceView.frame = self.sourceView.frame.withPosition(x: 0.0, y: 0.0)
            self.sourceView.backgroundColor = self.sourceBackground
            
            //Restore the backgrounds
            self.destinationView.backgroundColor = self.destinationBackground
            self.containerView.backgroundColor = UIColor.black
            
            //Complete the transition
            self.destination.viewController(self.destination, didGetPresentedBy: self.source)
            transitionContext.completeTransition(!transitionContext.transitionWasCancelled)

        }
    }
}

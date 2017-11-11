//
//  MWMainTabBarControllerAnimatedTransitioning.swift
//  myWatch
//
//  Created by Máté on 2017. 07. 09..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWMainTabBarControllerAnimatedTransitioning: NSObject, UIViewControllerAnimatedTransitioning
{
    var tabBarController: UITabBarController!
    var index: Int!
    
    func transitionDuration(using transitionContext: UIViewControllerContextTransitioning?) -> TimeInterval
    {
        return 0.15
    }
    
    func animateTransition(using transitionContext: UIViewControllerContextTransitioning)
    {
        let source: UIViewController = transitionContext.viewController(forKey: .from)!
        let destination: UIViewController = transitionContext.viewController(forKey: .to)!
        
        let sourceView: UIView = (source as? UINavigationController)?.topViewController?.view ?? source.view
        let destinationView: UIView = destination.view
        let containerView: UIView = transitionContext.containerView
        
        containerView.backgroundColor = MWDefaults.Colors.defaultBackgroundColor
        
        destinationView.alpha = 0.0
        containerView.addSubview(destinationView)
        
        let duration: TimeInterval = transitionDuration(using: transitionContext)
        
        UIView.animate(withDuration: duration / 2, delay: 0.0, options: .curveEaseIn, animations: { 
            sourceView.alpha = 0.0
        }, completion: nil)
        
        UIView.animate(withDuration: duration / 2, delay: duration / 2, options: .curveEaseOut, animations: { 
            destinationView.alpha = 1.0
        }) { (finished: Bool) in
            source.view.removeFromSuperview()
            sourceView.alpha = 1.0
            
            self.tabBarController.selectedIndex = self.index
            
            transitionContext.completeTransition(!transitionContext.transitionWasCancelled)
        }
    }
}

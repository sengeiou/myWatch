//
//  MWTabBarControllerSegue.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 28..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWTabBarControllerSegue: UIStoryboardSegue
{
    override func perform()
    {
        guard let destinationViewController = self.destination as? MWMainTabBarController else
        {
            fatalError("The destination view controller in segue \(self.identifier ?? "<MISSING SEGUE IDENTIFIER>")) must be an instance of MWMainTabBarController!")
        }
        
        let sourceView: UIView = self.source.view
        let destinationView: UIView = destinationViewController.view
        
        let window: UIWindow = UIApplication.shared.keyWindow!
        
        let destinationBackgroundColor: UIColor? = destinationView.backgroundColor
        
        window.backgroundColor = sourceView.backgroundColor
        sourceView.backgroundColor = UIColor.clear
        destinationView.backgroundColor = UIColor.clear
        
        destinationView.alpha = 0.0
        
        window.insertSubview(destinationView, aboveSubview: sourceView)
        
        let navigationController: UINavigationController = destinationViewController.viewControllers![0] as! UINavigationController
        let tabBar: MWTabBar = destinationViewController.tabBar as! MWTabBar
        let tabBarOriginalFrame: CGRect = tabBar.frame
        
        navigationController.setNavigationBarHidden(true, animated: false)
        
        tabBar.frame = tabBar.frame.offsetBy(dx: 0, dy: tabBar.frame.height + (tabBar._style == .custom ? 15.0 : 0.0))
        
        UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseInOut, animations: {
            window.backgroundColor = destinationBackgroundColor
            
            sourceView.frame = sourceView.frame.offsetBy(dx: -(window.frame.width / 2), dy: 0)
            sourceView.alpha = 0.0
        }) { (finished: Bool) in
            UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseOut, animations: {
                destinationView.alpha = 1.0
            }, completion: { (finished: Bool) in
                navigationController.navigationBar.alpha = 0.0
                tabBar.alpha = 0.0
                
                navigationController.setNavigationBarHidden(false, animated: true)
                
                UIView.animate(withDuration: 0.15, delay: 0.0, options: .curveLinear, animations: {
                    tabBar.frame = tabBarOriginalFrame
                    
                    navigationController.navigationBar.alpha = 1.0
                    tabBar.alpha = 1.0
                }, completion: { (finished: Bool) in
                    self.source.present(self.destination, animated: false, completion: nil)
                    sourceView.removeFromSuperview()
                })
            })
        }
    }
}

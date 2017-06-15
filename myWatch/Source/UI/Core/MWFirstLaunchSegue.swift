//
//  MWFirstLaunchSegue.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 20..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWFirstLaunchSegue: UIStoryboardSegue
{
    override init(identifier: String?, source: UIViewController, destination: UIViewController)
    {
        super.init(identifier: identifier ?? "<MISSING SEGUE IDENTIFIER>", source: source, destination: destination)
    }
    
    override func perform()
    {        
        if(self.source is MWFirstLaunchViewController)
        {
            if(self.destination is MWFirstLaunchViewController)
            {
                let sourceView: UIView = self.source.view
                let destinationView: UIView = self.destination.view
                let destinationBackground: UIColor? = destinationView.backgroundColor
                
                let window: UIWindow = UIApplication.shared.keyWindow!
                window.insertSubview(destinationView, aboveSubview: sourceView)
                
                window.backgroundColor = sourceView.backgroundColor
                sourceView.backgroundColor = UIColor.clear
                destinationView.backgroundColor = UIColor.clear
                
                destinationView.frame = destinationView.frame.offsetBy(dx: window.frame.width / 3, dy: 0)
                destinationView.alpha = 0.0
                
                let sourceImageBar: MWFirstLaunchImageBar = (self.source as! MWFirstLaunchViewController).getImageBar()
                let destinationImageBar: MWFirstLaunchImageBar = (self.destination as! MWFirstLaunchViewController).getImageBar()
                
                destinationImageBar.alpha = 0.0
                
                sourceView.translatesAutoresizingMaskIntoConstraints = true
                sourceView.subviews.forEach({ (subview: UIView) in
                    subview.translatesAutoresizingMaskIntoConstraints = true
                })
                
                window.insertSubview(sourceImageBar, belowSubview: destinationView)
                sourceImageBar.updateFrame()
                
                let destinationButton: MWButton? = (self.destination as! MWFirstLaunchViewController).getButton()
                
                MWUtil.execute(ifNotNil: destinationButton, execution: { 
                    destinationButton!.alpha = 0.0
                })
                
                sourceImageBar.animate(to: destinationImageBar, withDuration: 0.25)
                
                UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseInOut, animations: {
                    window.backgroundColor = destinationBackground
                    
                    sourceView.frame = sourceView.frame.offsetBy(dx: -(window.frame.width / 3), dy: 0)
                    destinationView.frame = destinationView.frame.offsetBy(dx: -(window.frame.width / 3), dy: 0)
                    
                    sourceView.alpha = 0.0
                    destinationView.alpha = 1.0
                }, completion: { (finished: Bool) in
                    destinationView.backgroundColor = destinationBackground
                    destinationImageBar.alpha = 1.0
                    self.source.present(self.destination, animated: false, completion: {
                        MWUtil.execute(ifNotNil: (self.destination as! MWFirstLaunchViewController).viewControllerDidGetPresented, execution: {
                            (self.destination as! MWFirstLaunchViewController).viewControllerDidGetPresented!()
                        })
                    })
                    
                    sourceImageBar.removeFromSuperview()
                    
                    MWUtil.execute(ifNotNil: destinationButton, execution: {
                        UIView.animate(withDuration: 0.15, delay: 0.0, options: .curveEaseOut, animations: {
                            destinationButton!.alpha = 1.0
                        }, completion: nil)
                    })
                })
            }
            else
            {
                fatalError("Expected destination view controller in segue with identifier: \(self.identifier!) to conform to protocol MWFirstLaunchViewController!")
            }
        }
        else
        {
            fatalError("Expected source view controller in segue with identifier: \(self.identifier!) to conform to protocol MWFirstLaunchViewController!")
        }
    }
}

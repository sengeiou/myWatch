//
//  MWSlideSegueWB.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 28..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWSlideSegueWB: UIStoryboardSegue
{
    override func perform()
    {
        //Declarations
        let sourceView = self.source.view!
        let destinationView = self.destination.view!
        
        guard let outerAppDestination = self.destination as? MWOuterAppViewController else
        {
            fatalError("The specified destination view controller in segue: \(self.identifier!) is not an outer app view controller.")
        }
        
        if(outerAppDestination.getButton() != nil)
        {
            let destinationButton = outerAppDestination.getButton()
            
            //Preparation
            let window = UIApplication.shared.keyWindow!
            window.backgroundColor = sourceView.backgroundColor
            
            let destinationBackground = destinationView.backgroundColor
            
            sourceView.backgroundColor = UIColor.clear
            destinationView.backgroundColor = UIColor.clear
            
            destinationView.frame = destinationView.frame.offsetBy(dx: window.frame.width / 3, dy: 0)
            window.insertSubview(destinationView, aboveSubview: sourceView)
            
            destinationButton!.alpha = 0.0
            
            //Animation
            UIView.animate(withDuration: 0.25, delay: 0.0, options: .curveEaseInOut, animations: {
                window.backgroundColor = destinationBackground
                
                sourceView.frame = sourceView.frame.offsetBy(dx: -(window.frame.width / 3), dy: 0)
                sourceView.alpha = 0.0
                
                destinationView.frame = destinationView.frame.offsetBy(dx: -(window.frame.width / 3), dy: 0)
                destinationView.alpha = 1.0
                
                destinationButton!.alpha = 0.0
            }, completion: { (finised: Bool) in
                UIView.animate(withDuration: 0.15, delay: 0.0, options: .curveLinear, animations: {
                    destinationButton!.alpha = 1.0
                }, completion: { (finised : Bool) in
                    destinationView.backgroundColor = destinationBackground
                    self.source.present(self.destination, animated: false, completion: nil)
                })
            })

        }
        else
        {
            fatalError("Expected destination view controller to have a button in segue: \(self.identifier!).")
        }
        
    }
}

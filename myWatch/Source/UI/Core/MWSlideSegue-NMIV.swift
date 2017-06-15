//
//  MWSlideSegue-NMIV.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 28..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWSlideSegueNMIV: MWSlideSegue
{
    private var window: UIWindow!
    private var sourceImageView: UIImageView!
    private var destinationImageView: UIImageView!
    
    override func perform()
    {
        super.setAnimationWillBeAddedLater()
        super.perform()
        
        //Declarations
        guard let outerAppSource = self.source as? MWOuterAppViewController else
        {
            fatalError("The specified source view controller in segue: \(self.identifier!) is not an outer app view controller.")
        }
        
        guard let outerAppDestination = self.destination as? MWOuterAppViewController else
        {
            fatalError("The specified destination view controller in segue: \(self.identifier!) is not an outer app view controller.")
        }
        
        sourceImageView = outerAppSource.getImageView()
        destinationImageView = outerAppDestination.getImageView()
        
        //Preparation
        window = UIApplication.shared.keyWindow!
        
        sourceImageView.image = destinationImageView.image
        
        if(!animationWillBeAddedLater)
        {
            //Animation
            UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseInOut, animations: {
                self.sourceImageView.frame = self.sourceImageView.frame.offsetBy(dx: self.window.frame.width / 3, dy: 0)
                self.destinationImageView.frame = self.destinationImageView.frame.offsetBy(dx: self.window.frame.width / 3, dy: 0)
                self.sourceImageView.alpha = 1.0
                self.destinationImageView.alpha = 1.0
            }, completion: nil)
            
            super.addAnimation()
        }
    }
    
    internal override func addAnimation()
    {
        UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseInOut, animations: {
            self.sourceImageView.frame = self.sourceImageView.frame.offsetBy(dx: self.window.frame.width / 3, dy: 0)
            self.destinationImageView.frame = self.destinationImageView.frame.offsetBy(dx: self.window.frame.width / 3, dy: 0)
            self.sourceImageView.alpha = 1.0
            self.destinationImageView.alpha = 1.0
        }, completion: nil)
        
        super.addAnimation()
    }
}

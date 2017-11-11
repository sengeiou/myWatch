//
//  MWViewController.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 21..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWViewController: UIViewController
{
    @IBInspectable var statusBarStyle: Int = 2
    {
        didSet
        {
            self.setNeedsStatusBarAppearanceUpdate()
        }
    }
    
    override var preferredStatusBarStyle: UIStatusBarStyle
    {
        return UIStatusBarStyle(rawValue: MWUtil.clamp(statusBarStyle - 1, min: 0, max: UIStatusBarStyle.count))!
    }
    
    override func viewDidLoad()
    {
        super.viewDidLoad()
        
        if(view.subviews.count <= 2) //The top- and bottom layout guides are always added to the view.
        {
            //Create the indicator label
            let viewControllerLabel: UILabel = UILabel()
            viewControllerLabel.translatesAutoresizingMaskIntoConstraints = false

            // - Prepare text
            viewControllerLabel.font = UIFont.systemFont(ofSize: 30.0, weight: UIFontWeightBlack)
            viewControllerLabel.text = self.title
            viewControllerLabel.textColor = UIColor.white
            viewControllerLabel.textAlignment = .center
            
            // - Prepare size
            viewControllerLabel.sizeToFit()
            
            // - Prepare autoshrink
            viewControllerLabel.adjustsFontSizeToFitWidth = true
            viewControllerLabel.minimumScaleFactor = 0.5

            // - Prepare alpha
            viewControllerLabel.alpha = 0.3
            
            // - Prepare position
            let l: NSLayoutConstraint = viewControllerLabel.leadingAnchor.constraint(equalTo: self.view.layoutMarginsGuide.leadingAnchor)
            let t: NSLayoutConstraint = viewControllerLabel.trailingAnchor.constraint(equalTo: self.view.layoutMarginsGuide.trailingAnchor)
            let c: NSLayoutConstraint = viewControllerLabel.centerYAnchor.constraint(equalTo: self.view.centerYAnchor)
            
            // - Add the label
            self.view.addSubview(viewControllerLabel)
            self.view.addConstraints([l, t, c])
        }
    }
    
    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
}

extension UIViewController
{
    /// Called by any custom transition when a view controller is presented.
    ///
    /// This function was intended to be overriden by subclasses.
    ///
    /// - Parameters:
    ///   - viewController: The view controller which is being presented.
    ///   - by: The presenting view controller.
    func viewController(_ viewController: UIViewController, didGetPresentedBy by: UIViewController)
    {
        //No-operation
    }
}

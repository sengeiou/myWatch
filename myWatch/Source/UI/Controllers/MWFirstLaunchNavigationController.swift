//
//  MWFirstLaunchNavigationController.swift
//  myWatch
//
//  Created by Máté on 2017. 07. 08..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWFirstLaunchNavigationController: UINavigationController, UINavigationControllerDelegate
{
    let firstLaunchAnimatedTransitioning: MWFirstLaunchAnimatedTransitioning = MWFirstLaunchAnimatedTransitioning()
    
    override func viewDidLoad()
    {
        super.viewDidLoad()
        self.delegate = self
    }
    
    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
    
    func navigationController(_ navigationController: UINavigationController, animationControllerFor operation: UINavigationControllerOperation, from fromVC: UIViewController, to toVC: UIViewController) -> UIViewControllerAnimatedTransitioning?
    {
        firstLaunchAnimatedTransitioning.reversed = operation == .pop
        return firstLaunchAnimatedTransitioning
    }
}

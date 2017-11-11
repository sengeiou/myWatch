//
//  MWMainTabBarController.swift
//  myWatch
//
//  Created by Máté on 2017. 06. 11..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWMainTabBarController: UITabBarController, UITabBarControllerDelegate
{
    private let mainTabBarControllerAnimatedTransitioning: MWMainTabBarControllerAnimatedTransitioning = MWMainTabBarControllerAnimatedTransitioning()
    
    override func viewDidLoad()
    {
        super.viewDidLoad()
        self.delegate = self
        mainTabBarControllerAnimatedTransitioning.tabBarController = self
    }
    
    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
    
    override func viewDidAppear(_ animated: Bool)
    {
        //Supercall
        super.viewDidAppear(animated)
    }
    
    override func tabBar(_ tabBar: UITabBar, didSelect item: UITabBarItem)
    {
        self.tabBar.items ?! {
            self.selectedViewController = self.viewControllers?[tabBar.items!.index(of: item) ?? tabBar.items!.count]
        }
    }
    
    func tabBarController(_ tabBarController: UITabBarController, animationControllerForTransitionFrom fromVC: UIViewController, to toVC: UIViewController) -> UIViewControllerAnimatedTransitioning?
    {
        mainTabBarControllerAnimatedTransitioning.index = self.viewControllers!.index(of: toVC) ?? self.viewControllers!.count //We assume that the view controllers array is not nil, because this function is called whenever we selet one from the array and we want to transition to it.
        return mainTabBarControllerAnimatedTransitioning
    }
}

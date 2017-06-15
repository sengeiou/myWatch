//
//  MWFirstLaunchNavigationController.swift
//  myWatch
//
//  Created by Máté on 2017. 06. 11..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWFirstLaunchNavigationController: UINavigationController
{
    override func viewDidLoad()
    {
        super.viewDidLoad()
    }

    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
    
    override func pushViewController(_ viewController: UIViewController, animated: Bool)
    {
        super.pushViewController(viewController, animated: animated)
    }
    
    override func popViewController(animated: Bool) -> UIViewController?
    {
        return super.popViewController(animated: animated)
    }
}

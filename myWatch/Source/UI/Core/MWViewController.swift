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
    internal var firstLaunchViewController: Bool = false
    
    override var preferredStatusBarStyle: UIStatusBarStyle
    {
        return .lightContent
    }
    
    override func viewDidLoad()
    {
        super.viewDidLoad()
        self.view.backgroundColor = MWDefaults.Colors.defaultBackgroundColor
    }
    
    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
}

//
//  MWFirstLaunchViewController.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 20..
//  Copyright © 2017. theMatys. All rights reserved.
//

import Foundation

@objc protocol MWFirstLaunchViewController
{
    func getImageBar() -> MWFirstLaunchImageBar
    
    func getButton() -> MWButton?
    
    @objc optional func viewControllerDidGetPresented()
}

//
//  MWOuterAppViewController.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 22..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWOuterAppViewController: MWViewController
{
    private var imageView: MWTintedImageView!
    private var button: MWButton?
    private var imageAnimation: MWImageAnimation?
    
    override func viewDidLoad()
    {
        super.viewDidLoad()
    }

    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
    
    internal func setImageView(_ imageView: MWTintedImageView)
    {
        self.imageView = imageView
    }
    
    internal func setButton(_ button: MWButton)
    {
        self.button = button
    }
    
    internal func setImageAnimation(_ imageAnimation: MWImageAnimation)
    {
        self.imageAnimation = imageAnimation
    }
    
    func getImageView() -> MWTintedImageView
    {
        return self.imageView
    }
    
    func getButton() -> MWButton?
    {
        return self.button
    }
    
    func getImageAnimation() -> MWImageAnimation?
    {
        return self.imageAnimation
    }
}

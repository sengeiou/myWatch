//
//  MWLanguageViewController.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 09..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWLanguageViewController: MWOuterAppViewController
{
    //MARK: Member variables
    @IBOutlet weak var imageViewIcon: MWTintedImageView!
    @IBOutlet weak var labelSceneTitle: UILabel!
    @IBOutlet weak var labelSceneDesc: UILabel!
    @IBOutlet weak var buttonForwarder: MWButton!
    @IBOutlet weak var stackViewContext: UIStackView!

    //MARK: Inherited functions from: UIViewController
    override func viewDidLoad()
    {
        super.viewDidLoad()
        
        self.setImageView(imageViewIcon)
    }
    
    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
    
    //MARK: Action functions
    @IBAction func buttonPressed_buttonAnimateImage(_ sender: MWButton)
    {
        let frames: [UIImage] = MWAssets.Images.Frames.framesConnect
        let duration: TimeInterval = Double(frames.count) / MWDefaults.Animation.defaultFramesPerSecond
        let animation: MWImageAnimation = MWImageAnimation(repeatCount: 1, duration: duration, frames: frames)
        
        imageViewIcon.attachImageAnimation(animation)
        imageViewIcon.image = frames.last
        
        imageViewIcon.startAnimating()
    }
}

//
//  MWNameViewController.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 20..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWNameViewController: MWOuterAppViewController, UITextFieldDelegate
{
    //MARK: Member variables
    @IBOutlet weak var imageViewIcon: MWTintedImageView!
    @IBOutlet weak var labelSceneTitle: UILabel!
    @IBOutlet weak var labelSceneDesc: UILabel!
    @IBOutlet weak var labelNote: UILabel!
    @IBOutlet weak var textFieldName: MWTextField!
    @IBOutlet weak var buttonForwarder: MWButton!
    @IBOutlet weak var stackViewContext: UIStackView!
    
    private var deviceName: String = ""
    
    private var keyboardShows: Bool = false
    {
        didSet
        {
            self.setNeedsStatusBarAppearanceUpdate()
        }
    }

    override var prefersStatusBarHidden: Bool
    {
        return keyboardShows
    }
    
    //MARK: - Inherited functions from: UIViewController
    override func viewDidLoad()
    {
        super.viewDidLoad()
        
        textFieldName.delegate = self
        
        NotificationCenter.default.addObserver(self, selector: #selector(MWNameViewController.keyboardWillShow), name: NSNotification.Name.UIKeyboardWillShow, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(MWNameViewController.keyboardWillHide), name: NSNotification.Name.UIKeyboardWillHide, object: nil)
        
        buttonForwarder.isEnabled = false
        
        self.setImageView(imageViewIcon)
        self.setButton(buttonForwarder)
    }

    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
    
    //MARK: Inherited functions from: UITextFieldDelegate
    func textFieldShouldReturn(_ textField: UITextField) -> Bool
    {
        textField.resignFirstResponder()
        return true
    }
    
    func textFieldDidEndEditing(_ textField: UITextField)
    {
        deviceName = textField.text!
        
        if(deviceName != "")
        {
            buttonForwarder.isEnabled = true
        }
    }
    
    //MARK: Private functions
    internal func keyboardWillShow(notification: NSNotification)
    {
        keyboardShows = true
        
        if let keyboardSize = (notification.userInfo?[UIKeyboardFrameBeginUserInfoKey] as? NSValue)?.cgRectValue
        {
            if self.view.frame.origin.y == 0
            {
                self.view.frame.origin.y -= keyboardSize.height
            }
        }
    }
    
    internal func keyboardWillHide(notification: NSNotification)
    {
        keyboardShows = false
        
        if let keyboardSize = (notification.userInfo?[UIKeyboardFrameBeginUserInfoKey] as? NSValue)?.cgRectValue
        {
            if self.view.frame.origin.y != 0
            {
                self.view.frame.origin.y += keyboardSize.height
            }
        }
    }
    
    //MARK: Action functions
    @IBAction func buttonPressed_buttonForwarder(_ sender: MWButton)
    {
        MWUtil.execute(ifNotNil: myWatch.getSettings().currentDevice) { 
            if(self.deviceName != "")
            {
                myWatch.getSettings().currentDevice!.setGivenName(self.deviceName)
            }
        }
                
        MWIO.save(myWatch.getSettings(), to: MWFileLocations.settingsFile)
    }
}

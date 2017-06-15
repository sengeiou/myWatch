//
//  MWNameDeviceController.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 20..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWNameDeviceController: MWViewController, MWFirstLaunchViewController, UITextFieldDelegate
{
    //MARK: Member variables
    @IBOutlet weak var imageBar: MWFirstLaunchImageBar!
    @IBOutlet weak var labelTitle: UILabel!
    @IBOutlet weak var labelDesc: UILabel!
    @IBOutlet weak var labelDesc_1: UILabel!
    @IBOutlet weak var textFieldDeviceName: MWTextField!
    @IBOutlet weak var buttonForwarder: MWButton!
    
    var device: MWDevice!
    private var deviceName: String?
    
    private var keyboardShows: Bool = false
    
    override var prefersStatusBarHidden: Bool
    {
        return keyboardShows
    }
    
    override var preferredStatusBarUpdateAnimation: UIStatusBarAnimation
    {
        return .slide
    }
    
    //MARK: - Inherited functions from: UIViewController
    override func viewDidLoad()
    {
        self.firstLaunchViewController = true
        super.viewDidLoad()
        
        textFieldDeviceName.delegate = self
        
        NotificationCenter.default.addObserver(self, selector: #selector(MWNameDeviceController.keyboardWillShow), name: NSNotification.Name.UIKeyboardWillShow, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(MWNameDeviceController.keyboardWillHide), name: NSNotification.Name.UIKeyboardWillHide, object: nil)
        
        buttonForwarder.disableButton()
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
        deviceName = textField.text
        
        MWUtil.execute(ifNotNil: deviceName) { 
            self.buttonForwarder.enableButton()
        }
    }
    
    //MARK: Private functions
    internal func keyboardWillShow(notification: NSNotification)
    {
        keyboardShows = true
        self.setNeedsStatusBarAppearanceUpdate()
        
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
        self.setNeedsStatusBarAppearanceUpdate()
        
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
        device.givenName = deviceName! //The device name should not be nil, because we only allow to press the button when the textfield has a proper name in it.
        myWatch.get().settings.currentDevice = device
    }
    
    //MARK: Inherited functions from: MWFirstLaunchViewController
    func getImageBar() -> MWFirstLaunchImageBar
    {
        return self.imageBar
    }
    
    func getButton() -> MWButton?
    {
        return self.buttonForwarder
    }
}

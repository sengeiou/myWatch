//
//  MWNameDeviceViewController.swift
//  myWatch
//
//  Created by Máté on 2017. 07. 16.
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

/// A view controller which asks the user to name the device which they have selected to connect to.
class MWNameDeviceViewController: MWViewController, UITextFieldDelegate
{
    //MARK: Outlets
    @IBOutlet weak var labelTitle: UILabel!
    @IBOutlet weak var labelDescription: UILabel!
    @IBOutlet weak var labelDescription_1: UILabel!
    @IBOutlet weak var textFieldDeviceName: MWTextField!
    @IBOutlet weak var labelDescription_2: UILabel!
    
    @IBOutlet weak var barButtonNext: UIBarButtonItem!
    
    //MARK: Instance variables
    
    /// The device which the application is conected to.
    private var device: MWDevice = MWSettings.shared.device //Should not be nil at this point (this view controller is only presented if the user has chosen an available device).
    
    //MARK: - Inherited functions from: MWViewController
    override func viewDidLoad()
    {
        //Supercall
        super.viewDidLoad()
        
        //Set this class as the text field's delegate
        textFieldDeviceName.delegate = self
        
        //Check if the device already has a name - if it does, display it in the text field
        if(device.name != "")
        {
            textFieldDeviceName.text = device.name
            barButtonNext.isEnabled = true
        }
    }

    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
    
    //MARK: Inhertied functions from: UITextFieldDelegate
    func textFieldShouldReturn(_ textField: UITextField) -> Bool
    {
        //Resign text field's first responder status
        return textField.resignFirstResponder()
    }
    
    func textFieldDidEndEditing(_ textField: UITextField)
    {
        //Set the the name of the application's current device
        device.name = textField.text!
    }
    
    //MARK: Action functions
    @IBAction func buttonPressed_barButtonNext(_ sender: UIBarButtonItem)
    {
        //Resign text field's first responder status
        let _ = textFieldDeviceName.resignFirstResponder()
    }
    
    @IBAction func editingChanged_textFieldDeviceName(_ sender: MWTextField)
    {
        sender.text ??! {
            barButtonNext.isEnabled = sender.text!.characters.count >= 1
        } >< {
            barButtonNext.isEnabled = false
        }
    }
}

//
//  MWAppleHealthViewController.swift
//  myWatch
//
//  Created by Máté on 2017. 07. 16.
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

/// A view controller which asks the user to enable/disable exporting application data to Apple Health.
class MWAppleHealthViewController: MWViewController
{
    //MARK: Outlets
    @IBOutlet weak var labelTitle: UILabel!
    @IBOutlet weak var labelDescription: UILabel!
    @IBOutlet weak var labelDescription_1: UILabel!
    @IBOutlet weak var labelDescription_2: UILabel!
    
    @IBOutlet weak var buttonYes: MWButton!
    @IBOutlet weak var buttonNo: MWButton!
    
    //MARK: Instance variables
    private var navigationBarTintColor: UIColor?
    
    //MARK: - Inherited funtions from: MWViewController
    override func viewDidLoad()
    {
        super.viewDidLoad()
        
        //Check if the controller has a navigation controller
        self.navigationController ?! {
            //Store the navigation bar's tint color
            navigationBarTintColor = self.navigationController!.navigationBar.tintColor
        }
    }

    override func viewWillAppear(_ animated: Bool)
    {
        //Supercall
        super.viewWillAppear(animated)
        
        //Check if we have a stored navigation bar tint color
        navigationBarTintColor ?! {
            UIView.transition(with: self.navigationController!.navigationBar, duration: 0.2, options: .transitionCrossDissolve, animations: {
                //Set a custom navigation bar tint color
                self.navigationController!.navigationBar.tintColor = self.labelTitle.textColor
            }, completion: nil)
        }
    }
    
    override func viewWillDisappear(_ animated: Bool)
    {
        //Supercall
        super.viewWillDisappear(animated)
        
        //Check if we have a stored navigation bar tint color
        navigationBarTintColor ?! {
            UIView.transition(with: self.navigationController!.navigationBar, duration: 0.2, options: .transitionCrossDissolve, animations: { 
                //Restore the navigation bar's tint color
                self.navigationController!.navigationBar.tintColor = self.navigationBarTintColor!
            }, completion: nil)
        }
    }
    
    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
    
    //MARK: Action functions
    @IBAction func buttonPressed_buttonYes(_ sender: MWButton)
    {
        MWSettings.shared.exportToAppleHealth = true
        self.performSegue(withIdentifier: MWIdentifiers.SegueIdentifiers.appleHealthToFirstLaunchLast, sender: self)
    }
    
    @IBAction func buttonPressed_buttonNo(_ sender: MWButton)
    {
        MWSettings.shared.exportToAppleHealth = false
        self.performSegue(withIdentifier: MWIdentifiers.SegueIdentifiers.appleHealthToFirstLaunchLast, sender: self)
    }
}

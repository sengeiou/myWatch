//
//  MWAppleHealthController.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 20..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWAppleHealthController: MWViewController, MWFirstLaunchViewController
{
    //MARK: Member variables
    @IBOutlet weak var imageBar: MWFirstLaunchImageBar!
    @IBOutlet weak var labelTitle: UILabel!
    @IBOutlet weak var labelDesc: UILabel!
    @IBOutlet weak var buttonEnable: MWButton!
    @IBOutlet weak var buttonDisable: MWButton!
    
    //MARK: - Inherited functions from: MWViewController
    override func viewDidLoad()
    {
        super.viewDidLoad()
    }

    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
    
    //MARK: Inherited functions from: MWFirstLaunchViewController
    func getImageBar() -> MWFirstLaunchImageBar
    {
        return self.imageBar
    }
    
    func getButton() -> MWButton?
    {
        return nil
    }
    
    //MARK: Action functions
    @IBAction func buttonPressed_buttonEnable(_ sender: MWButton)
    {
        myWatch.get().settings.exportToAppleHealth = true
    }
    
    @IBAction func buttonPressed_buttonDisable(_ sender: MWButton)
    {
        myWatch.get().settings.exportToAppleHealth = false
        
        self.performSegue(withIdentifier: MWIdentifiers.SegueIdentifiers.appleHealthToFirstLaunchLast, sender: self)
    }
}

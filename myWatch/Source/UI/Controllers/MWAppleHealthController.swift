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
    @IBOutlet weak var labelAppleHealth: UILabel!
    @IBOutlet weak var switchAppleHealth: UISwitch!
    @IBOutlet weak var buttonForwarder: MWButton!
    
    //MARK: - Inherited functions from: MWViewController
    override func viewDidLoad()
    {
        self.firstLaunchViewController = true
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
        return self.buttonForwarder
    }
    
    //MARK: Action functions
    @IBAction func buttonPressed_buttonForwarder(_ sender: MWButton)
    {
        myWatch.get().settings.exportToAppleHealth = switchAppleHealth.isOn
    }
}

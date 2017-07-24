//
//  MWFirstLaunchBaseControllers.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 20..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWFirstLaunchFirstController: MWViewController, MWFirstLaunchController, MWFirstLaunchViewController
{
    //MARK: Member variables
    @IBOutlet weak var imageBar: MWFirstLaunchImageBar!
    @IBOutlet weak var labelTitle: UILabel!
    @IBOutlet weak var labelDesc: UILabel!
    @IBOutlet weak var labelDesc_1: UILabel!
    @IBOutlet weak var labelButtonHeader: UILabel!
    @IBOutlet weak var buttonForwarder: MWButton!
    
    //MARK: - Inherited functions from: MWViewController
    override func viewDidLoad()
    {
        super.viewDidLoad()
        
        buttonForwarder.staysHighlighted = true
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
    
    //MARK: Inherited functions form: MWFirstLaunchControllerProtocol
    func getFirstLaunchImageBar() -> MWFirstLaunchImageBar!
    {
        return imageBar
    }
}

class MWFirstLaunchLastController: MWViewController, MWFirstLaunchController, MWFirstLaunchViewController
{
    //MARK: Member variables
    @IBOutlet weak var imageBar: MWFirstLaunchImageBar!
    @IBOutlet weak var labelTitle: UILabel!
    @IBOutlet weak var labelDesc: UILabel!
    @IBOutlet weak var labelDesc_1: UILabel!
    @IBOutlet weak var buttonForwarder: MWButton!
    
    //MARK: - Inherited functions from: MWViewController
    override func viewDidLoad()
    {
        super.viewDidLoad()
        
        buttonForwarder.staysHighlighted = true
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
    
    //MARK: Inherited functions form: MWFirstLaunchControllerProtocol
    func getFirstLaunchImageBar() -> MWFirstLaunchImageBar!
    {
        return imageBar
    }
}

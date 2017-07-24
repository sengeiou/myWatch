//
//  MWConnectingController.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 27..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWConnectingController: MWViewController, MWFirstLaunchController, MWFirstLaunchViewController, MWBCommunicatorDelegate
{
    //MARK: Member variables
    @IBOutlet weak var imageBar: MWFirstLaunchImageBar!
    @IBOutlet weak var labelTitle: MWLabel!
    
    var device: MWDevice!
    
    //MARK: - Inherited functions from: MWViewController
    override func viewDidLoad()
    {
        super.viewDidLoad()
    }
    
    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
    
    //MARK: Inherited functions form: MWFirstLaunchControllerProtocol
    func getFirstLaunchImageBar() -> MWFirstLaunchImageBar!
    {
        return imageBar
    }
    
    //MARK: Inherited functions from: MWBCommunicatorDelegate
    func bluetoothCommunicator(_ communicator: MWBCommunicator, didConnectToDevice device: MWDevice)
    {
        /* No-operation */
    }
    
    func bluetoothCommunicator(_ communicator: MWBCommunicator, didFinishPreparationsForDevice device: MWDevice)
    {
        if(self.isViewLoaded)
        {
            self.performSegue(withIdentifier: MWIdentifiers.SegueIdentifiers.connectingToNameDevice, sender: self)
        }
    }
    
    // MARK: Navigation functions
    override func prepare(for segue: UIStoryboardSegue, sender: Any?)
    {
        super.prepare(for: segue, sender: sender)
        
        var destination: MWNameDeviceController!
        MWUtil.downcast(to: &destination, from: segue.destination)
        
        destination.device = device
    }
    
    func getImageBar() -> MWFirstLaunchImageBar
    {
        return self.imageBar
    }
    
    func getButton() -> MWButton?
    {
        return nil
    }
    
    func viewControllerDidGetPresented()
    {
        myWatch.get().bluetoothCommunicator.delegate = self
        myWatch.get().bluetoothCommunicator.attemptToConnect(to: device)
    }
}

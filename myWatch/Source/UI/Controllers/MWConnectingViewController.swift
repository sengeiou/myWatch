//
//  MWConnectingViewController.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 18..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWConnectingViewController: MWOuterAppViewController, MWBCommunicatorDelegate
{
    //MARK: Member variables
    @IBOutlet weak var imageViewIcon: MWTintedImageView!
    @IBOutlet weak var labelConnecting: UILabel!
    @IBOutlet weak var activityIndicator: UIActivityIndicatorView!
    @IBOutlet weak var stackViewContext: UIStackView!
    
    //MARK: - Inherited functions from: UIViewController
    override func viewDidLoad()
    {
        super.viewDidLoad()
        
        MWUtil.execute(ifNotNil: myWatch.getSettings().currentDevice) { 
            MWUtil.execute(ifNil: myWatch.getSettings().currentDevice!.getPeripheral(), execution: { 
                myWatch.getBluetoothCommunicator().initializeBluetooth(withDelegate: self, withDevice: myWatch.getSettings().currentDevice!)
            }, elseExecution: {
                myWatch.getBluetoothCommunicator().changeDelegate(to: self)
            })
        }
 
        self.setImageView(imageViewIcon)
    }
    
    override func viewDidAppear(_ animated: Bool)
    {
        super.viewDidAppear(animated)
        
        MWUtil.execute(ifNotNil: myWatch.getSettings().currentDevice) { 
            MWUtil.execute(ifNotNil: myWatch.getSettings().currentDevice!.getPeripheral(), execution: { 
                myWatch.getBluetoothCommunicator().attemptToConnect(to: myWatch.getSettings().currentDevice!)
            })
        }
    }
    
    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }

    //MARK: Inherited functions from: MWBCommunicatorDelegate
    func bluetoothHasFoundSpecifiedDevice()
    {
        /* No-operation */
    }
    
    func connectionSuccessful(to device: MWDevice)
    {
        /* No-operation */
    }
    
    func deviceIsReadyToUse(device: MWDevice)
    {
        MWUtil.execute(ifNotNil: myWatch.getSettings().currentDevice) { 
            if(device == myWatch.getSettings().currentDevice!)
            {
                if(device.getGivenName() == "")
                {
                    self.performSegue(withIdentifier: "MWSIDConnectingToNameDevice", sender: self)
                }
                else
                {
                    self.performSegue(withIdentifier: "MWSIDConnectingToMain", sender: self)
                }
            }
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?)
    {
        super.prepare(for: segue, sender: sender)
    }
}

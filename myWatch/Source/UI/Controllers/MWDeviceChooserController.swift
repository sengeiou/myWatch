//
//  MWDeviceChooserController.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 20..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWDeviceChooserController: MWViewController, MWFirstLaunchViewController, MWBCommunicatorDelegate, UITableViewDataSource
{
    //MARK: Member variables
    @IBOutlet weak var imageBar: MWFirstLaunchImageBar!
    @IBOutlet weak var labelTitle: UILabel!
    @IBOutlet weak var labelDesc: UILabel!
    @IBOutlet weak var tableViewDevices: UITableView!
    @IBOutlet weak var stackViewSearching: UIStackView!
    @IBOutlet weak var labelNoBluetooth: MWLabel!
    @IBOutlet weak var buttonForwarder: MWButton!
    @IBOutlet weak var buttonNext: UIBarButtonItem!
    
    private var avaiableDevices: [MWDevice] = [MWDevice]()
    
    fileprivate var selectedDevice: MWDevice?
    {
        didSet
        {
            buttonNext.isEnabled = selectedDevice != nil
            buttonForwarder.isEnabled = selectedDevice != nil
        }
    }
    
    //MARK: - Inherited functions from: MWViewController
    override func viewDidLoad()
    {
        self.firstLaunchViewController = true
        super.viewDidLoad()
        
        buttonNext.isEnabled = false
        buttonForwarder.disableButton()
        
        setupTableView()
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
    
    func viewControllerDidGetPresented()
    {
        setupBluetooth()
    }
    
    //MARK: Inherited functions from: MWBCommunicatorDelegate
    func bluetoothHasBeenEnabled()
    {
        UIView.animate(withDuration: 0.1, delay: 0.0, options: .curveEaseOut, animations: { 
            self.stackViewSearching.alpha = 1.0
        }, completion: nil)
        
        UIView.animate(withDuration: 0.1, delay: 0.0, options: .curveEaseIn, animations: { 
            self.labelNoBluetooth.alpha = 0.0
        }, completion: nil)
        
        myWatch.get().bluetoothCommunicator.lookForDevices()
    }
    
    func bluetoothNotAvailable()
    {
        UIView.animate(withDuration: 0.1, delay: 0.0, options: .curveEaseOut, animations: {
            self.labelNoBluetooth.alpha = 1.0
        }, completion: nil)
    }
    
    func bluetoothHasFoundDevice(_ device: MWDevice)
    {
        avaiableDevices.append(device)
        let indexPath: IndexPath = IndexPath(row: avaiableDevices.count - 1, section: 0)
        tableViewDevices.insertRows(at: [indexPath], with: .fade)
    }
    
    //MARK: Inherted functions from: UITableViewDataSource
    func numberOfSections(in tableView: UITableView) -> Int
    {
        return 1
    }
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int
    {
        return avaiableDevices.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell
    {
        guard let cell = tableView.dequeueReusableCell(withIdentifier: MWIdentifiers.CellIdentifiers.deviceChooserDeviceCell, for: indexPath) as? MWDeviceChooserDeviceCell else
        {
            fatalError("The dequed cell is not an instance of \"MWDeviceCell\".")
        }
        
        cell.prepare(device: avaiableDevices[indexPath.row], controller: self)
        
        return cell
    }
    
    //MARK: Private functions
    private func setupTableView()
    {
        tableViewDevices.dataSource = self
        tableViewDevices.tableFooterView = UIView(frame: .zero)
    }
    
    private func setupBluetooth()
    {
        myWatch.get().bluetoothCommunicator.initializeBluetooth(withDelegate: self)
    }
    
    //MARK: Navigation functions
    override func prepare(for segue: UIStoryboardSegue, sender: Any?)
    {
        super.prepare(for: segue, sender: sender)
        
        var destination: MWConnectingController!
        MWUtil.downcast(to: &destination, from: segue.destination)
        
        destination.device = selectedDevice! //It should not be nil, because we only allow to forward when there is a device selected from the list.
    }
}

//MARK: -
class MWDeviceChooserDeviceCell: UITableViewCell
{
    //MARK: Member variables
    @IBOutlet weak var imageViewIcon: MWTintedImageView!
    @IBOutlet weak var labelText: UILabel!
    @IBOutlet weak var line: UIView!
    
    private var device: MWDevice!
    private var controller: MWDeviceChooserController?
    private var color: UIColor = UIColor.white
    
    //MARK: - Instance functions
    func prepare(device: MWDevice, controller: MWDeviceChooserController)
    {
        self.device = device
        self.controller = controller
        
        MWUtil.safelySetValue(&labelText.text, toValue: labelText.text! + device.deviceID)
        
        if(controller.tableViewDevices.visibleCells.count >= 1)
        {
            line.alpha = 0.0
        }
        else
        {
            line.backgroundColor = controller.tableViewDevices.separatorColor
        }
    }
    
    //MARK: Inherited functions from: UITableViewCell
    internal override func setSelected(_ selected: Bool, animated: Bool)
    {
        if(controller != nil)
        {
            if(selected)
            {
                UIView.animate(withDuration: 0.5, delay: 0.0, options: .curveEaseOut, animations: {
                    self.color = MWDefaults.Colors.defaultTintColor
                    
                    self.imageViewIcon.setTintingColor(self.color)
                    self.labelText.textColor = self.color
                }, completion: nil)
                
                controller!.selectedDevice = device
            }
            else
            {
                UIView.animate(withDuration: 0.5, delay: 0.0, options: .curveEaseOut, animations: {
                    self.color = UIColor.white
                    
                    self.imageViewIcon.setTintingColor(self.color)
                    self.labelText.textColor = self.color
                }, completion: nil)
                
                controller!.selectedDevice = nil
            }
        }
    }
}

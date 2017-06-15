//
//  MWDeviceChooserViewController.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 28..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWDeviceChooserViewController: MWOuterAppViewController, MWBCommunicatorDelegate, UITableViewDataSource
{
    //MARK: Member variables
    @IBOutlet weak var imageViewIcon: MWTintedImageView!
    @IBOutlet weak var labelSceneTitle: UILabel!
    @IBOutlet weak var labelSceneDesc: UILabel!
    @IBOutlet weak var tableViewDevices: UITableView!
    @IBOutlet weak var activityIndicator: UIActivityIndicatorView!
    @IBOutlet weak var labelSearching: UILabel!
    @IBOutlet weak var buttonForwarder: MWButton!
    
    private var avaiableDevices: [MWDevice] = [MWDevice]()
    
    fileprivate var selectedDevice: MWDevice?
    {
        didSet
        {
            buttonForwarder.isEnabled = selectedDevice != nil
        }
    }
    
    //MARK: - Inherited functions from: MWViewController
    override func viewDidLoad()
    {
        super.viewDidLoad()
        buttonForwarder.isEnabled = false
        
        let duration: TimeInterval = Double(MWAssets.Images.Frames.framesConnect.count) / MWDefaults.Animation.defaultFramesPerSecond
        let animation: MWImageAnimation = MWImageAnimation(repeatCount: 1, duration: duration, frames: MWAssets.Images.Frames.framesConnect)
        
        self.setImageView(imageViewIcon)
        self.setButton(buttonForwarder)
        self.setImageAnimation(animation)
        
        setupBluetooth()
        setupTableView()
    }

    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
    
    //MARK: Inherited functions from: MWBCommunicatorDelegate
    func bluetoothHasBeenEnabled()
    {
        myWatch.getBluetoothCommunicator().lookForDevices()
    }
    
    func bluetoothHasFoundDevice(device: MWDevice)
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
        guard let cell = tableView.dequeueReusableCell(withIdentifier: MWIdentifiers.CellIdentifiers.deviceCell, for: indexPath) as? MWDeviceCell else
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
        myWatch.getBluetoothCommunicator().initializeBluetooth(withDelegate: self)
    }
    
    //MARK: Action functions
    @IBAction func buttonPressed_buttonForwarder(_ sender: MWButton)
    {
        MWUtil.execute(ifNotNil: selectedDevice, execution: {
            myWatch.getSettings().currentDevice = self.selectedDevice
        })
    }
}

//MARK: -
class MWDeviceCell: UITableViewCell
{
    //MARK: Member variables
    @IBOutlet weak var imageViewIcon: MWTintedImageView!
    @IBOutlet weak var labelText: UILabel!
    
    private var device: MWDevice!
    private var controller: MWDeviceChooserViewController?
    private var color: UIColor = UIColor.white
    
    //MARK: - Instance functions
    func prepare(device: MWDevice, controller: MWDeviceChooserViewController)
    {
        self.device = device
        self.controller = controller
        
        MWUtil.safelySetValue(&labelText.text, toValue: labelText.text! + device.getDeviceID())
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

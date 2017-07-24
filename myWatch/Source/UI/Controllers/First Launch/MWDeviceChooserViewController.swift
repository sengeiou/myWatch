//
//  MWDeviceChooserViewController.swift
//  myWatch
//
//  Created by Máté on 2017. 07. 15.
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

/// A view controller which lists all available myWatch Bluetooth devices and connects to the one chosen by the user.
class MWDeviceChooserViewController: MWViewController, UINavigationControllerDelegate, UITableViewDelegate, UITableViewDataSource, MWBCommunicatorDelegate
{
    //MARK: Outlets
    @IBOutlet weak var labelTitle: UILabel!
    @IBOutlet weak var labelDescription: UILabel!
    @IBOutlet weak var labelDescription_1: UILabel!
    @IBOutlet weak var tableViewDevices: UITableView!
    @IBOutlet weak var stackViewSearchIndicator: UIStackView!
    @IBOutlet weak var labelNoBluetooth: UILabel!
    
    //MARK: Instance variables
    
    /// An array with the devices that Bluetooth found compatible and available.
    private var availableDevices: [MWDevice] = [MWDevice]()
    
    /// An array with all the index paths for the cells of available devices.
    ///
    /// Used to remove all the cells when Bluetooth is turned off.
    private var availableDevicesIndexPaths: [IndexPath] = [IndexPath]()
    
    /// The selected device from the table view.
    ///
    /// The value of this variable should be in array `availableDevices`
    private var selectedDevice: MWDevice?
    
    /// The cell of the selected device.
    ///
    /// Used to finalize the cell when transitioning forwards with a `push` operation.
    private var selectedDeviceIndexPath: IndexPath?
    
    /// A boolean indicating whether the Bluetooth is available.
    ///
    /// Used to determine whether the controller is able to look for devices when it is presented.
    ///
    /// This is important, because if we are popping back to this controller, the search process is not started by default. (The communicator automatically turns off the search process when connecting to a device.)
    private var bluetoothAvailable: Bool!
    {
        didSet
        {
            updateNoBluetoothScreen()
        }
    }
    
    /// The image view which displays a Bluetooth icon on the No-Bluetooth screen.
    private var imageViewNoBluetooth: MWImageView!
    
    /// The visual effect view which serves as the background for the No-Bluetooth screen.
    private var visualEffectView: UIVisualEffectView!
    
    /// The normal tint color of the navigation bar.
    ///
    /// This variable serves as a backup of the original tint color, because in certain scenarios, we "disable" the navigation bar (change its tint color and disable user interaction).
    private var navigationBarTintColor: UIColor?
    
    //MARK: - Inherited functions from: MWViewController
    override func viewDidLoad()
    {
        //Supercall
        super.viewDidLoad()
        
        //Setup table view
        tableViewDevices.delegate = self
        tableViewDevices.dataSource = self
        
        //Store the navigation bar's default tint color
        navigationBarTintColor = self.navigationController?.navigationBar.tintColor
        
        //Setup the 'Bluetooth not available' screen
        // - Make the visual effect view
        visualEffectView = UIVisualEffectView(frame: self.view.frame)
        visualEffectView.isHidden = true
        visualEffectView.effect = UIBlurEffect(style: .dark)
        
        // - Make the image view
        imageViewNoBluetooth = MWImageView()
        imageViewNoBluetooth.silently().tintingColor = UIColor.white
        imageViewNoBluetooth.image = MWAssets.Images.imageGeneralBluetooth.getImage(in: Bundle(for: type(of: self)), traits: self.traitCollection)
        imageViewNoBluetooth.sizeToFit()
        imageViewNoBluetooth.center.x = visualEffectView.center.x
        imageViewNoBluetooth.center.y = visualEffectView.center.y - (imageViewNoBluetooth.frame.height / 2) - 8.0
        
        // - Update the label
        labelNoBluetooth.isHidden = false
        
        visualEffectView.addSubview(imageViewNoBluetooth)
        labelNoBluetooth.transfer(to: visualEffectView)
        
        //Add the whole screen
        self.view.addSubview(visualEffectView)
    
        //If we have popped this controller, but have already connected to a device, display the connected device in the table view
        //Otherwise, initialize the Bluetooth
        myWatch.get().settings.currentDevice ??! {
            selectedDevice = myWatch.get().settings.currentDevice
            selectedDeviceIndexPath = addCell(for: myWatch.get().settings.currentDevice)
            
            availableDevicesIndexPaths.append(selectedDeviceIndexPath!)
            
            bluetoothAvailable = true
        } >< {
            myWatch.get().bluetoothCommunicator.initializeBluetooth(with: self)
        }
    }

    override func viewWillAppear(_ animated: Bool)
    {
        //Supercall
        super.viewWillAppear(animated)
        
        //Set this class the navigation controller's delegate
        self.navigationController?.delegate = self
        
        //Potentially show the No-Bluetooth screen if the Bluetooth is not available
        updateNoBluetoothScreen()
        
        //If we are popping back to this view controller, or presenting it with a currently selected device, add the device to the available devices
        selectedDevice ?! {
            if(!availableDevices.contains(selectedDevice!))
            {
                availableDevices.append(selectedDevice!)
                availableDevicesIndexPaths.append(selectedDeviceIndexPath!)
            }
        }
    }
    
    override func viewDidDisappear(_ animated: Bool)
    {
        //Supercall
        super.viewDidDisappear(animated)
        
        //Enable table view controller user interaction
        tableViewDevices.isUserInteractionEnabled = true
        
        //Reset the table view, but leave the selected cell in it
        reset(false)
    }
    
    override func viewWillLayoutSubviews()
    {
        //Supercall
        super.viewWillLayoutSubviews()
        
        //Lay out the No-Bluetooth screen
        // - Lay out the visual effect view
        visualEffectView.frame = self.view.frame
        
        // - Lay out the image view
        imageViewNoBluetooth.center.x = self.view.center.x
        imageViewNoBluetooth.center.y = self.view.center.y - (imageViewNoBluetooth.frame.height / 2) - 8
        
        // - Lay out the label
        labelNoBluetooth.translatesAutoresizingMaskIntoConstraints = true
        labelNoBluetooth.center.x = self.view.center.x
        labelNoBluetooth.center.y = self.view.center.y + (labelNoBluetooth.frame.height / 2) + 8
    }
    
    override func didReceiveMemoryWarning()
    {
        super.didReceiveMemoryWarning()
    }
    
    //MARK: Inherited functions from: UINavigationControllerDelegate
    func navigationController(_ navigationController: UINavigationController, animationControllerFor operation: UINavigationControllerOperation, from fromVC: UIViewController, to toVC: UIViewController) -> UIViewControllerAnimatedTransitioning?
    {
        //Get the transitioning from the navigation controller
        let firstLaunchAnimatedTransitioning: MWFirstLaunchAnimatedTransitioning = (self.navigationController as? MWFirstLaunchNavigationController)?.firstLaunchAnimatedTransitioning ?? MWFirstLaunchAnimatedTransitioning()
        
        //Set the animation to be reversed if we are popping this view controller
        firstLaunchAnimatedTransitioning.reversed = operation == .pop
        
        //Check if we have to hide the No-Bluetooth screen before performing the transition
        if(!visualEffectView.isHidden)
        {
            //Save the effect of the visual effect view
            let effect: UIVisualEffect? = visualEffectView.effect
            
            //Hide the screen before the transition's animation
            firstLaunchAnimatedTransitioning.explicitControl = { (transitionContext: UIViewControllerContextTransitioning) in
                //Animate the hiding process
                UIView.animate(withDuration: 0.1, delay: 0.0, options: .curveEaseOut, animations: {
                    self.imageViewNoBluetooth.alpha = 0.0
                    self.labelNoBluetooth.alpha = 0.0
                    
                    self.visualEffectView.effect = nil
                }, completion: { (finished: Bool) in
                    //Finally hide the screen and restore the effect
                    self.visualEffectView.isHidden = true
                    self.visualEffectView.effect = effect
                    
                    //Execute the transition's animation
                    firstLaunchAnimatedTransitioning.executeAnimation(using: transitionContext)
                })
            }
        }
        
        //If we are transitioning forwards (aka. if we are pushing), prepare the views for a possible rewind
        if(operation == .push)
        {
            //Enable the navigation bar
            updateNavigationBar(true)
            
            //Restore the navigation controller's delegate
            self.navigationController?.delegate = self.navigationController as? MWFirstLaunchNavigationController
        }

        //Return the transition
        return firstLaunchAnimatedTransitioning
    }
    
    //MARK: Inherited functions from: UITableViewDataSource
    
    func numberOfSections(in tableView: UITableView) -> Int
    {
        return 1
    }
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int
    {
        return availableDevices.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell
    {
        //Dequeue a cell
        guard let cell = tableView.dequeueReusableCell(withIdentifier: MWIdentifiers.TableViewCellIdentifiers.deviceChooserDevice, for: indexPath) as? MWDeviceChooserDeviceCell else
        {
            //If the dequeue operation fails, assert
            fatalError("Unable to dequeue reusable cell with reuse identifier \"\(MWIdentifiers.TableViewCellIdentifiers.deviceChooserDevice)\": the dequued cell is not an instance of \"MWDeviceChooserDeviceCell\"")
        }
        
        //Prepare the cell and finalize it immediately if the cell is for an already selected device
        cell.prepare(device: availableDevices[indexPath.row], isCellWithSelectedDevice: availableDevices[indexPath.row] == selectedDevice)
        
        //Return the cell
        return cell
    }
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath)
    {
        //Get the cell for the row
        guard let cell = tableView.cellForRow(at: indexPath) as? MWDeviceChooserDeviceCell else
        {
            //If the operation fails, assert
            fatalError("The cell for row \(indexPath.row) in the device chooser table view is not an instance of \"MWDeviceChosserDeviceCell\"")
        }
        
        //Check if the controller should connect to the cell's device or just forward
        if(!cell.isCellWithSelectedDevice)
        {
            //If there is a cell currently selected, unselect and reset it
            selectedDevice ?! {
                //Get the cell
                guard let selectedCell = tableView.cellForRow(at: selectedDeviceIndexPath!) as? MWDeviceChooserDeviceCell else
                {
                    //If the operation fails, assert
                    fatalError("The cell for row \(indexPath.row) in the device chooser table view is not an instance of \"MWDeviceChosserDeviceCell\"")
                }
                
                ////Reset the selected cell's look for possible reuse
                selectedCell.update(false)
            }
            
            //Set the new selected device and its index path
            selectedDevice = cell.device
            selectedDeviceIndexPath = indexPath
            
            //Disable the navigation bar
            updateNavigationBar(false)
            
            //Hide the search indicator
            updateSearchIndicator(true)
            
            //Disable table view user interaction
            tableView.isUserInteractionEnabled = false
            
            //Attempt to connect to the device
            myWatch.get().bluetoothCommunicator.attemptToConnect(to: selectedDevice!)
        }
        else
        {
            //Forward the first launch setup
            forward()
        }
    }
    
    //MARK: Inherited functions from: MWBCommunicatorDelegate
    func bluetoothCommunicator(_ communicator: MWBCommunicator, didUpdateBluetoothAvailability availability: Bool)
    {
        //Update the availability
        bluetoothAvailable = availability
    }
    
    func bluetoothCommunicator(_ communicator: MWBCommunicator, didFindCompatibleDevice device: MWDevice)
    {
        //Create a cell for the newfound device
        let newIndexPath: IndexPath? = addCell(for: device)
        
        newIndexPath ?! {
            availableDevicesIndexPaths.append(newIndexPath!)
        }
    }
    
    func bluetoothCommunicator(_ communicator: MWBCommunicator, didConnectToDevice device: MWDevice)
    {
        //No-operation
    }
    
    func bluetoothCommunicator(_ communicator: MWBCommunicator, didFinishPreparationsForDevice device: MWDevice)
    {
        //Set application's device to the currently selected device,
        myWatch.get().settings.currentDevice = device
        
        //Get the selected cell
        guard let selectedCell = tableViewDevices.cellForRow(at: selectedDeviceIndexPath!) as? MWDeviceChooserDeviceCell else
        {
            //If the operation fails, assert
            fatalError("The cell for row \(selectedDeviceIndexPath!.row) in the device chooser table view is not an instance of \"MWDeviceChosserDeviceCell\"")
        }
        
        //Update the selected cell's look to indicate that the application has connected to its device
        selectedCell.update(true)
        
        //Forward the first launch setup
        forward()
    }
    
    /// Adds a cell to the table view designed for the specified device and returns the cell's index path
    ///
    /// - Parameter device: The device which the cell should represent.
    func addCell(for device: MWDevice) -> IndexPath?
    {
        if(!availableDevices.contains(device))
        {
            let indexPath: IndexPath = IndexPath(row: availableDevices.count, section: 0)
            
            availableDevices.append(device)
            tableViewDevices.insertRows(at: [indexPath], with: .fade)
            
            return indexPath
        }
        else
        {
            return nil
        }
    }
    
    /// Performs the segue which forwards the first launch setup.
    fileprivate func forward()
    {
        performSegue(withIdentifier: MWIdentifiers.SegueIdentifiers.deviceChooserToNameDevice, sender: self)
    }
    
    /// "Disables" or "enables" the navigation bar based on the specified parameter.
    ///
    /// - Parameter enabled: The boolean which indicates whether the navigation bar should be disabled.
    private func updateNavigationBar(_ enabled: Bool)
    {
        //Check if the navigation bar should be enabled
        if(enabled)
        {
            //If it should, restore the tint color and enable user interaction
            self.navigationController?.navigationBar.tintColor = navigationBarTintColor ?? MWDefaults.Colors.defaultTintColor
            self.navigationController?.navigationBar.isUserInteractionEnabled = true
        }
        else
        {
            //If it should not, set the tint color to a disabled color (light gray) and disable user interaction
            self.navigationController?.navigationBar.tintColor = UIColor.lightGray
            self.navigationController?.navigationBar.isUserInteractionEnabled = false
        }
    }
    
    /// Shows or hides the No-Bluetooth screen and the search indicator based on the current Bluetooth state.
    private func updateNoBluetoothScreen()
    {
        bluetoothAvailable ?! {
            //Check if the Bluetooth is available
            if(self.bluetoothAvailable)
            {
                //Check if the No-Bluetooth screen is showing
                if(!visualEffectView.isHidden)
                {
                    //Save the effect for the visual effect view
                    let effect: UIVisualEffect? = visualEffectView.effect
                    
                    //Animate the screen out
                    UIView.animate(withDuration: 0.1, delay: 0.0, options: .curveEaseIn, animations: {
                        self.imageViewNoBluetooth.alpha = 0.0
                        self.labelNoBluetooth.alpha = 0.0
                    }, completion: { (finished: Bool) in
                        UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseIn, animations: {
                            self.visualEffectView.effect = nil
                        }, completion: { (finished: Bool) in
                            //Finally hide the visual effect view and restore its effect
                            self.visualEffectView.isHidden = true
                            self.visualEffectView.effect = effect
                            
                            //Show the search indicator
                            self.updateSearchIndicator(false)
                        })
                    })
                }
                else
                {
                    //Show the search indicator
                    updateSearchIndicator(false)
                }
                
                //Start the scan for devices
                myWatch.get().bluetoothCommunicator.lookForDevices()
            }
            else
            {
                //Prepare the screen contents
                imageViewNoBluetooth.alpha = 0.0
                labelNoBluetooth.isHidden = false
                
                //Prepare the visual effect view
                let effect: UIVisualEffect? = visualEffectView.effect
                visualEffectView.effect = nil
                visualEffectView.isHidden = false
                
                //Animate the screen in
                UIView.animate(withDuration: 0.2, delay: 0.35, options: .curveEaseOut, animations: {
                    self.visualEffectView.effect = effect
                }, completion: { (finished: Bool) in
                    UIView.animate(withDuration: 0.1, delay: 0.0, options: .curveEaseOut, animations: {
                        self.imageViewNoBluetooth.alpha = 1.0
                        self.labelNoBluetooth.alpha = 1.0
                    }, completion: nil)
                })
                
                //Hide the search indicator
                updateSearchIndicator(true)
                
                //Enable the navigation bar
                updateNavigationBar(true)
                
                //Reset the table view
                reset(true)
            }
        }
    }
    
    /// Shows or hides the search indicator based on the specified parameter.
    ///
    /// - Parameter hidden: The boolean which indicates whether the search indicator should be hidden.
    private func updateSearchIndicator(_ hidden: Bool)
    {
        UIView.animate(withDuration: 0.35, delay: 0.0, options: hidden ? .curveEaseIn : .curveEaseOut, animations: { 
            self.stackViewSearchIndicator.isHidden = hidden
        }, completion: nil)
    }
    
    /// Erases all cells from the table view either including or excluding the selected cell.
    ///
    /// - Parameter includingSelectedCell: The boolean which indicates that the selected cell should be erased from the table view
    private func reset(_ includingSelectedCell: Bool)
    {
        if(includingSelectedCell)
        {
            //Reset the devices
            selectedDevice = nil
            
            //If there is a device that the application is using, reset that, because an entire reconnection will be necessary
            myWatch.get().settings.currentDevice ?! {
                myWatch.get().settings.currentDevice = nil
            }

            //Reset the Bluetooth communicator's device
            myWatch.get().bluetoothCommunicator.device = nil
            
            //Remove all the available devices
            availableDevices.removeAll()
                        
            //Delete the cells and reset the table view
            tableViewDevices.deleteRows(at: availableDevicesIndexPaths, with: .fade)
            tableViewDevices.isUserInteractionEnabled = true
            
            //Remove all the cell index paths
            availableDevicesIndexPaths.removeAll()
            selectedDeviceIndexPath = nil
        }
        else
        {
            selectedDevice ?! {
                //Remove all the available devices
                availableDevices.removeAll()
                
                let excludingSelected: [IndexPath] = availableDevicesIndexPaths - selectedDeviceIndexPath!
                
                //Delete the cells and reset the table view
                if(!excludingSelected.isEmpty)
                {
                    tableViewDevices.deleteRows(at: excludingSelected, with: .fade)
                }
                
                tableViewDevices.isUserInteractionEnabled = true
                
                //Remove all the cell index paths
                availableDevicesIndexPaths.removeAll()
            }
        }
    }
}

//MARK: -

/// A table view cell for the table view which lists all the available myWatch Bluetooth devices in view controller `MWDeviceChooserViewController`.
class MWDeviceChooserDeviceCell: MWTableViewCell
{
    //MARK: Outlets
    @IBOutlet weak var imageViewIcon: MWImageView!
    @IBOutlet weak var labelDeviceName: UILabel!
    @IBOutlet weak var activityIndicator: UIActivityIndicatorView!
    
    //MARK: Instance variables
    
    /// The device this cell represents.
    ///
    /// The cell will append this device's ID to its label whenever this variable's value changes.
    fileprivate var device: MWDevice!
    
    /// A boolean which indicates that this cell's device is the one which the application is connected to.
    ///
    /// When set to `true`, tapping on the cell will simply forward the first launch setup. In this case, the cell displays a checkmark as its accessory to indicate that the application is connected to the device it represents.
    ///
    /// When set to `false`, tapping on the cell will make the application connect to the cell's device and if the connection succeeds, it will also forward the first launch setup. In this case, before the selection of the cell, the cell displays a disclosure indicator as its accessory, and after the selection, it displays an activity indicator until the device is ready to use.
    fileprivate var isCellWithSelectedDevice: Bool = false
    
    //MARK: - Inherited functions from: MWActionTableViewCell
    override func setSelected(_ selected: Bool, animated: Bool)
    {
        super.silently().setSelected(selected, animated: true)
        
        //Check if the cell is bound to be selected
        if(selected)
        {
            //Check if the cell's device is the application's current device
            if(!isCellWithSelectedDevice)
            {
                //If it is not, display the activity indicator
                self.accessoryType = .none
                activityIndicator.startAnimating()
                activityIndicator.isHidden = false
                activityIndicator.alpha = 1.0
                
                //The cell's controller will automatically handle the connection to the cell's device
            }
        }
    }
    
    //MARK: Instance functions
    
    /// Prepares the cell for use.
    ///
    /// - Parameters:
    ///   - device: The device the cell should represent and connect to upon selection.
    ///   - isCellWithSelectedDevice: The boolean which indicates that the cell's device is the application's current device. 
    ///                               
    ///     Set to `true` if we already have a connected device when pushing the cell's view controller on the top of the navigation stack.
    func prepare(device: MWDevice, isCellWithSelectedDevice: Bool)
    {
        //Check if the cell already has a device
        self.device ??= {
            //If it does not have one, append the new device's ID to the cell's label and store the device
            labelDeviceName.text = labelDeviceName.text?.appending(device.deviceID)
            
            self.device = device
        } >< {
            //If it does have one, check if the new one differs from the current one
            if(self.device != device)
            {
                //Remove the old device's ID from the cell's label and append the new one's ID
                //NOTE: At this point, the label's text should not be nil, because we already have a device ID attached to it.
                labelDeviceName.text!.removeSubrange(labelDeviceName.text!.index(labelDeviceName.text!.endIndex, offsetBy: -12)..<labelDeviceName.text!.endIndex)
                labelDeviceName.text!.append(device.deviceID)
                
                //Store the new device
                self.device = device
            }
        }
        
        //Update the cell's look based on the "isCellWithSelectedDevice" parameter
        update(isCellWithSelectedDevice)
    }
    
    /// Updates the cell's look to either the selected or the normal look.
    ///
    /// - Parameter isCellWithSelectedDevice: The boolean which indicates whether the cell's device is the application's current device (whether the cell is selected).
    func update(_ isCellWithSelectedDevice: Bool)
    {
        //Check if the cell represents the application's selected device
        if(isCellWithSelectedDevice)
        {
            //Update the cell's look to the selected look
            // - Hide the activity indicator
            activityIndicator.isHidden = true
            activityIndicator.alpha = 0.0
            activityIndicator.stopAnimating()
            
            // - Display a checkmark accessory
            self.accessoryType = .checkmark
        }
        else
        {
            //Update the cell's look to the normal look
            // - Display a disclosure indicator accessory
            self.accessoryType = .disclosureIndicator
        }
        
        //Store the cell's state
        self.isCellWithSelectedDevice = isCellWithSelectedDevice
    }
}

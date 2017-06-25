//
//  myWatch.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 09..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

/// The application's main class.
class myWatch
{
    //MARK: Instance variables
    
    /// The shared settings of the application.
    ///
    /// - See: `MWSettings` for more details.
    var settings: MWSettings = MWSettings()
    
    /// The main Bluetooth communicator of the application.
    var bluetoothCommunicator: MWBCommunicator = MWBCommunicator()
   
    /// The boolean which identicates whether debug mode should be used in the application.
    var debugMode: Bool = false

    //MARK: Static variables
    
    /// The singleton instance of the `myWatch` class.
    private static var instance: myWatch = myWatch()
    
    //MARK: Initializers
    
    /// Required for the singleton instance.
    private init() {}

    //MARK: Static functions
    
    /// Used to retrieve the singleton instance of `myWatch`.
    ///
    /// - Returns: The singleton instance of `myWatch`.
    static func get() -> myWatch
    {
        return instance
    }
}

//MARK: -
@UIApplicationMain
fileprivate class MWApplicationDelegate: UIResponder, UIApplicationDelegate
{
    //MARK: Instance variables
    
    /// The `UIWindow` of the application.
    ///
    /// Required for `UIApplicationDelegate`.
    var window: UIWindow?
    
    //MARK: - Inherited functions from: UIApplicationDelegate
    internal func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]?) -> Bool
    {
        //launchSetup()
        myWatch.get().debugMode = false
        return true
    }
    
    internal func applicationWillResignActive(_ application: UIApplication)
    {
        // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
        // Use this method to pause ongoing tasks, disable timers, and invalidate graphics rendering callbacks. Games should use this method to pause the game.
    }
    
    internal func applicationDidEnterBackground(_ application: UIApplication)
    {
        // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later.
        // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
    }
    
    internal func applicationWillEnterForeground(_ application: UIApplication)
    {
        // Called as part of the transition from the background to the active state; here you can undo many of the changes made on entering the background.
    }
    
    internal func applicationDidBecomeActive(_ application: UIApplication)
    {
        // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
    }
    
    internal func applicationWillTerminate(_ application: UIApplication)
    {
        MWIO.save(myWatch.get().settings, to: MWFileLocations.settingsFile)
    }
    
    //MARK: Instance functions
    
    /// Determines whether the application is launched for the first time.
    ///
    /// If the application is launched for the first time, it prepares the first launch.
    ///
    /// If the application is not launched for the first time, it loads the settings and prepares a regular launch.
    private func launchSetup()
    {
        //Attempt ot load the settings
        let loadedSettings: MWSettings? = MWIO.load(from: MWFileLocations.defaultSaveLocation)
        
        //Check whether the load was successful
        MWUtil.execute(ifNil: loadedSettings, execution: {
            //If it was not, create the myWatch directory and settings file
            if(!FileManager().fileExists(atPath: MWFileLocations.defaultSaveLocation.path))
            {
                do
                {
                    try FileManager().createDirectory(atPath: MWFileLocations.defaultSaveLocation.path, withIntermediateDirectories: false, attributes: nil)
                }
                catch let error as NSError
                {
                    MWLError("Unable to create myWatch directory: \(error.localizedDescription)", module: .moduleCore)
                }
            }
            
            //Create an empty settings instance
            myWatch.get().settings = MWSettings()
            
            //Prepare for the first launch
            let storyboard: UIStoryboard = UIStoryboard(name: "Main", bundle: nil)
            let firstLaunchViewController: UIViewController = storyboard.instantiateViewController(withIdentifier: MWIdentifiers.SceneIdentifiers.firstLaunchFirst)
            
            self.window!.rootViewController = firstLaunchViewController
        }) {
            //If it was, set the settings to the loaded settings.
            myWatch.get().settings = loadedSettings!
        }
    }
}

//MARK: -

/// The shared settings of the application.
///
/// Written to an own settings file upon termination.
///
/// Attempted to be read from the file upon lauch.
///
/// The success of the reading process determines whether the application is launched for the first time.
///
/// - See: `launchSetup()` in `MWApplicationDelegate`.
internal class MWSettings: NSObject, NSCoding
{
    //MARK: Instance variables
    
    /// Holds the current device that the application uses to retrieve its data.
    var currentDevice: MWDevice!
    
    /// Holds a boolean which determines whether the application should be exporting its data to Apple Health.
    var exportToAppleHealth: Bool = false
    
    //MARK: - Inherited intializers fomr: NSCoding
    required init?(coder aDecoder: NSCoder)
    {
        //Decode properties
        self.currentDevice = aDecoder.decodeObject(forKey: PropertyKey.currentDevice) as! MWDevice
        self.exportToAppleHealth = aDecoder.decodeObject(forKey: PropertyKey.exportToAppleHealth) as! Bool
    }
    
    //MARK: Initializers
    
    /// Basic initializer for creating an empty instance for first launch.
    override init()
    {
        /* No-operation */
    }
    
    //MARK: Inherited functions from: NSCoding
    func encode(with aCoder: NSCoder)
    {
        //Encode properties
        aCoder.encode(currentDevice, forKey: PropertyKey.currentDevice)
        aCoder.encode(exportToAppleHealth, forKey: PropertyKey.exportToAppleHealth)
    }
    
    //MARK: -
    
    /// The structure which holds the property names used in the files to identify the properties of this object.
    private struct PropertyKey
    {
        //MARK: Property keys
        static let currentDevice: String = "MWPCurrentDevice"
        static let exportToAppleHealth: String = "MWPExportToAppleHealth"
    }
}


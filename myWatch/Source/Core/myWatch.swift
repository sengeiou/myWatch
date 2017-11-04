//
//  myWatch.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 09..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

/// A boolean which indicates whether the application is being launched for the first time.
fileprivate var firstLaunch: Bool = false

/// The application's main class.
class myWatch
{
    //MARK: Instance variables

    /// A boolean which indicates whether debug mode should be used in the application.
    var debugMode: Bool = false
    
    //MARK: Static variables
    
    /// The singleton instance of the `myWatch` class.
    static let shared: myWatch = myWatch()
    
    //MARK: Initializers
    
    /// Required for the singleton instance.
    private init() {}
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
    
    private var settings: MWSettings = MWSettings.shared
    
    //MARK: - Inherited functions from: UIApplicationDelegate
    internal func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]?) -> Bool
    {
        /*if(firstLaunch)
        {
            firstLaunch()
        }*/
        
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
        MWIO.save(settings, to: MWFileLocations.settingsFile)
    }
    
    //MARK: Instance functions
    
    /// Determines whether the application is launched for the first time.
    ///
    /// If the application is launched for the first time, it prepares the first launch.
    ///
    /// If the application is not launched for the first time, it loads the settings and prepares a regular launch.
    private func firstLaunch()
    {
        //Prepare for the first launch
        let storyboard: UIStoryboard = UIStoryboard(name: "myWatch", bundle: Bundle(for: type(of: self)))
        let firstLaunchViewController: UIViewController = storyboard.instantiateViewController(withIdentifier: MWIdentifiers.ControllerIdentifiers.firstLaunchNC)
        
        self.window!.rootViewController = firstLaunchViewController
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
    var device: MWDevice!
    
    /// Holds a boolean which determines whether the application should be exporting its data to Apple Health.
    var exportToAppleHealth: Bool = false
    
    //MARK: Static variables
    
    /// The (shared) singleton instance of `MWSettings`.
    static let shared: MWSettings = create()
    
    //MARK: - Inherited intializers fomr: NSCoding
    required internal init?(coder aDecoder: NSCoder)
    {
        //Decode properties
        self.device = aDecoder.decodeObject(forKey: PropertyKey.device) as! MWDevice
        self.exportToAppleHealth = aDecoder.decodeObject(forKey: PropertyKey.exportToAppleHealth) as! Bool
    }
    
    //MARK: Initializers
    
    /// Basic initializer for creating an empty instance for first launch.
    override private init()
    {
        /* No-operation */
    }
    
    //MARK: Inherited functions from: NSCoding
    func encode(with aCoder: NSCoder)
    {
        //Encode properties
        aCoder.encode(device, forKey: PropertyKey.device)
        aCoder.encode(exportToAppleHealth, forKey: PropertyKey.exportToAppleHealth)
    }
    
    //MARK: Static functions
    
    /// Either creates an empty settings instance or loads the settings from a saved settings file.
    ///
    /// - Returns: An `MWSettings` instance.
    private static func create() -> MWSettings
    {
        //Create a default return value
        let ret: MWSettings = MWSettings()
        
        /*//Attempt to load the settings
        let loadedSettings: MWSettings? = MWIO.load(from: MWFileLocations.defaultSaveLocation)
        
        //Check whether the load was successful
        loadedSettings ??= {
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
            
            //Set the application to first launch mode
            firstLaunch = true
        } >< {
            //If it was, set the settings to the loaded settings
            ret = loadedSettings!
        }*/
        
        return ret
    }
    
    //MARK: -
    
    /// The structure which holds the property names used in the files to identify the properties of this object.
    private struct PropertyKey
    {
        //MARK: Prefixes
        
        /// The prefix of the property keys.
        private static let prefix: String = "MWSettings"
        
        //MARK: Property keys
        static let device: String = prefix + "Device"
        static let exportToAppleHealth: String = prefix + "ExportToAppleHealth"
    }
}


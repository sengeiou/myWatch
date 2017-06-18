//
//  myWatch.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 09..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class myWatch
{
    private static var instance: myWatch = myWatch()
    
    //MARK: Member variables
    var settings: MWSettings = MWSettings()
    var bluetoothCommunicator: MWBCommunicator = MWBCommunicator()
    var debugMode: Bool = false
    
    //MARK: Instance functions
    private init() {}

    //MARK: Static functions
    static func get() -> myWatch
    {
        return instance
    }
}

//MARK: -
@UIApplicationMain
internal class myWatchApplicationDelegate: UIResponder, UIApplicationDelegate
{
    //MARK: Member variables
    var window: UIWindow?
    private let main: myWatch = myWatch.get()
    
    //MARK: - Inherited functions from: UIApplicationDelegate
    internal func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]?) -> Bool
    {
        //launchSetup()
        //main.debugMode = true
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
        MWIO.save(main.settings, to: MWFileLocations.settingsFile)
    }
    
    //MARK: Private functions
    private func launchSetup()
    {
        let loadedSettings: MWSettings? = MWIO.load(from: MWFileLocations.defaultSaveLocation)
        
        MWUtil.execute(ifNil: loadedSettings, execution: { 
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
            
            self.main.settings = MWSettings()
            
            let storyboard: UIStoryboard = UIStoryboard(name: "Main", bundle: nil)
            let firstLaunchViewController: UIViewController = storyboard.instantiateViewController(withIdentifier: MWIdentifiers.SceneIdentifiers.firstLaunchFirst)
            
            self.window!.rootViewController = firstLaunchViewController
        }) { 
            self.main.settings = loadedSettings!
        }
    }
}

//MARK: -
internal class MWSettings: NSObject, NSCoding
{
    //MARK: Member variables
    var currentDevice: MWDevice!
    var exportToAppleHealth: Bool = false
    
    //MARK: Instance functions
    override init()
    {
        /* No-operation */
    }
    
    //MARK: - Coding
    required init?(coder aDecoder: NSCoder)
    {
        self.currentDevice = aDecoder.decodeObject(forKey: PropertyKey.currentDevice) as! MWDevice
        self.exportToAppleHealth = aDecoder.decodeObject(forKey: PropertyKey.exportToAppleHealth) as! Bool
    }
    
    func encode(with aCoder: NSCoder)
    {
        aCoder.encode(currentDevice, forKey: PropertyKey.currentDevice)
        aCoder.encode(exportToAppleHealth, forKey: PropertyKey.exportToAppleHealth)
    }
    
    //MARK: -
    private struct PropertyKey
    {
        //MARK: Property keys
        static let currentDevice: String = "MWPCurrentDevice"
        static let exportToAppleHealth: String = "MWPExportToAppleHealth"
    }
}


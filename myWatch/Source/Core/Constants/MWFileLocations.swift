//
//  MWLocations.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 20..
//  Copyright © 2017. theMatys. All rights reserved.
//

import Foundation

/// Holds all the file locations used in the application.
struct MWFileLocations
{
    /// The default saving directory of myWatch.
    static let defaultSaveLocation = FileManager().urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("myWatch", isDirectory: true)
    
    /// The file the settings are saved to.
    static let settingsFile = defaultSaveLocation.appendingPathComponent("myWatchSettings")
}

//
//  MWLocations.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 20..
//  Copyright © 2017. theMatys. All rights reserved.
//

import Foundation

struct MWFileLocations
{
    static let defaultSaveLocation = FileManager().urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("myWatch", isDirectory: true)
    static let settingsFile = defaultSaveLocation.appendingPathComponent("myWatchSettings")
}

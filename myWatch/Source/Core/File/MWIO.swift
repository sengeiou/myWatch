//
//  MWIO.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 21..
//  Copyright © 2017. theMatys. All rights reserved.
//

import Foundation

/// Used to handle writing/reading from a file.
class MWIO
{
    //MARK: Static functions
    
    /// Used to save objects to files.
    ///
    /// Displays a message based on the success of the saving process.
    ///
    /// - Parameters:
    ///   - object: The object to save. Must inherit `NSCoding`.
    ///   - file: The URL of the file to save the object to.
    static func save(_ object: Any, to file: URL)
    {
        let save: Bool = NSKeyedArchiver.archiveRootObject(object, toFile: file.path)
        
        if(save)
        {
            MWLInfo("Saving object to: \"\(file.path)\" was successful.", module: .moduleIO)
        }
        else
        {
            MWLError("Failed to save object to: \"\(file.path)\".", module: .moduleIO)
        }
    }
    
    /// Used to load objects from files.
    ///
    /// Displays a message based on the success of the loading process.
    ///
    /// - Parameter file: The file to read the object from.
    /// - Returns: The object read from the file.
    static func load<ObjectType>(from file: URL) -> ObjectType?
    {        
        if let ret: ObjectType = NSKeyedUnarchiver.unarchiveObject(withFile: file.path) as? ObjectType
        {
            MWLInfo("Loading object from: \"\(file.path)\" was successful.", module: .moduleIO)
            return ret
        }
        else
        {
            MWLError("Unable to load object from file: \"\(file.path)\". Returning nil.", module: .moduleIO)
            return nil
        }
    }
}

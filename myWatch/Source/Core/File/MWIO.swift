//
//  MWIO.swift
//  myWatch
//
//  Created by Máté on 2017. 04. 21..
//  Copyright © 2017. theMatys. All rights reserved.
//

import Foundation

class MWIO
{
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
    
    static func load<ObjectType>(from file: URL) -> ObjectType?
    {
        let ret: ObjectType? = NSKeyedUnarchiver.unarchiveObject(withFile: file.path) as? ObjectType
        
        if(ret != nil)
        {
            MWLInfo("Loading object from: \"\(file.path)\" was successful.", module: .moduleIO)
            return ret!
        }
        else
        {
            MWLError("Unable to load object from file: \"\(file.path)\". Returning nil.", module: .moduleIO)
            return nil
        }
    }
}

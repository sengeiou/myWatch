//
//  myWatchTests.swift
//  myWatchTests
//
//  Created by Máté on 2017. 04. 09..
//  Copyright © 2017. theMatys. All rights reserved.
//

import XCTest
@testable import myWatch

class myWatchTests: XCTestCase
{
    func testErrorMessage()
    {
        let titleNil: String? = nil
        let title: String? = "Today"
        
        print("MWTabBar only accepts items of type: MWTabBarItem. The tab bar has received an item \(titleNil != nil ? "(with title: \(titleNil!))" : "") which is not an instance of this type. This tab bar item will be ignored and removed from the \"items\" array of this tab bar!")
        print("MWTabBar only accepts items of type: MWTabBarItem. The tab bar has received an item \(title != nil ? "(with title: \(title!))" : "") which is not an instance of this type. This tab bar item will be ignored and removed from the \"items\" array of this tab bar!")
    }
}

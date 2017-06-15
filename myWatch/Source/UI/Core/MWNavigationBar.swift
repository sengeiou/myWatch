//
//  MWNavigationBar.swift
//  myWatch
//
//  Created by Máté on 2017. 06. 10..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

@IBDesignable
class MWNavigationBar: UINavigationBar
{
    override init(frame: CGRect)
    {
        super.init(frame: frame)
        self.barTintColor = MWDefaults.Colors.defaultBackgroundColor
        self.barStyle = .blackTranslucent
        self.setBackgroundImage(UIImage(), for: .default)
        self.shadowImage = UIImage()
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        self.barTintColor = MWDefaults.Colors.defaultBackgroundColor
        self.barStyle = .blackTranslucent
        self.setBackgroundImage(UIImage(), for: .default)
        self.shadowImage = UIImage()
    }
}

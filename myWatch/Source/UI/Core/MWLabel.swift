//
//  MWLabel.swift
//  myWatch
//
//  Created by Máté on 2017. 05. 21..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

@IBDesignable
class MWLabel: UILabel
{
    @IBInspectable var style: Int = 0
    {
        didSet
        {
            setupLabel()
        }
    }
    
    @IBInspectable var fontSize: CGFloat = 18.0
    {
        didSet
        {
            setupLabel()
        }
    }
    
    @IBInspectable var customFont: Bool = false
    {
        didSet
        {
            setupLabel()
        }
    }
    
    override init(frame: CGRect)
    {
        super.init(frame: frame)
        setupLabel()
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        setupLabel()
    }
    
    private func setupLabel()
    {
        if(!customFont)
        {
            switch style
            {
            case 0:
                self.font = UIFont.systemFont(ofSize: fontSize, weight: UIFontWeightRegular)
                self.textColor = UIColor.white
            case 1:
                self.font = UIFont.systemFont(ofSize: fontSize, weight: UIFontWeightThin)
                self.textColor = UIColor.white
            case 2:
                self.font = UIFont.systemFont(ofSize: fontSize, weight: UIFontWeightSemibold)
                self.textColor = UIColor.white
            case 3:
                self.font = UIFont.systemFont(ofSize: 30.0, weight: UIFontWeightLight)
                self.textColor = MWDefaults.Colors.defaultTintColor
            default:
                break
            }
        }
    }
}


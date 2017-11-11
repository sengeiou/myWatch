//
//  MWNavigationBar.swift
//  myWatch
//
//  Created by Máté on 2017. 06. 27..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

@IBDesignable
class MWNavigationBar: UINavigationBar
{
    @IBInspectable var style: Int = 1
    {
        didSet
        {
            if(style < 1)
            {
                _style = MWNavigationBarStyle(rawValue: 0)!
            }
            else if(style > MWNavigationBarStyle.count)
            {
                _style = MWNavigationBarStyle(rawValue: MWNavigationBarStyle.count - 1)!
            }
            else
            {
                _style = MWNavigationBarStyle(rawValue: style - 1)!
            }
        }
    }
    
    private var _style: MWNavigationBarStyle = .system
    {
        didSet
        {
            _init()
        }
    }
    
    private var shadowLayer: CALayer = CALayer()
    private var clippingLayer: CALayer = CALayer()
    
    private var removedSeparatorLine: Bool = false
    
    override init(frame: CGRect)
    {
        super.init(frame: frame)
        
        _init()
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        
        _init()
    }
    
    override func layoutSubviews()
    {
        super.layoutSubviews()
        self.layoutIfNeeded()
    }
    
    override func layoutIfNeeded()
    {
        if(_style == .custom)
        {
            if(!removedSeparatorLine)
            {
                if let separatorLine = getSeparatorLine(for: self)
                {
                    separatorLine.isHidden = true
                    removedSeparatorLine = true
                }
            }
            
            clippingLayer.frame = self.bounds.offsetBy(dx: 0.0, dy: self.bounds.height).withSize(width: self.bounds.width, height: 30.0)
            
            let shadowPath = UIBezierPath(rect: self.bounds.scaleBy(width: -10.0, height: 0.0))
            shadowLayer.frame = clippingLayer.bounds.offsetBy(dx: 0.0, dy: -(self.bounds.height))
            
            shadowLayer.shadowOffset = CGSize(width: 5.0, height: 4.0)
            shadowLayer.shadowPath = shadowPath.cgPath
        }
    }
    
    private func _init()
    {        
        switch _style
        {
        case .system:
            break
            
        case .custom:
            self.layer.masksToBounds = false
            
            clippingLayer.frame = self.bounds.offsetBy(dx: 0.0, dy: self.bounds.height).withSize(width: self.bounds.width, height: 30.0)
            clippingLayer.masksToBounds = true
            
            let shadowPath = UIBezierPath(rect: self.bounds.scaleByCentered(width: -10.0, height: 0.0))
            shadowLayer.frame = clippingLayer.bounds.offsetBy(dx: 0.0, dy: -(self.bounds.height))

            shadowLayer.shadowColor = UIColor.black.cgColor
            shadowLayer.shadowRadius = 7.0
            shadowLayer.shadowOpacity = 0.5
            shadowLayer.shadowOffset = CGSize(width: 5.0, height: 4.0)
            shadowLayer.shadowPath = shadowPath.cgPath
            shadowLayer.masksToBounds = false
            
            clippingLayer.addSublayer(shadowLayer)
            self.layer.addSublayer(clippingLayer)
            
            break
                        
        case .firstLaunch:
            self.barTintColor = MWDefaults.Colors.defaultBackgroundColor
            self.setBackgroundImage(UIImage(), for: .default)
            self.shadowImage = UIImage()
            
            break
        }
    }
    
    override func pushItem(_ item: UINavigationItem, animated: Bool)
    {
        super.pushItem(item, animated: animated)
        
        getSeparatorLine(for: self)?.isHidden = true
    }
    
    private func getSeparatorLine(for view: UIView) -> UIImageView?
    {
        if(view is UIImageView && view.frame.height <= 1)
        {
            return view as? UIImageView
        }
        
        for subview in view.subviews
        {
            if let shadowImage = getSeparatorLine(for: subview)
            {
                return shadowImage
            }
        }
        
        return nil
    }
}

enum MWNavigationBarStyle: Int
{
    case system
    case custom
    case firstLaunch
    
    static var count: Int
    {
        return self.firstLaunch.hashValue + 1
    }
}

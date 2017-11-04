//
//  MWTableViewCellNew.swift
//  myWatch
//
//  Created by Máté on 2017. 07. 31..
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

@IBDesignable
class MWTableViewCellNew: UITableViewCell
{
    //MARK: Inspectables
    @IBInspectable var toggleMode: Bool = false
    
    @IBInspectable private var _appearance: Int = 1
    {
        didSet
        {
            appearance = MWTableViewCellAppearance(rawValue: MWUtil.clamp(_appearance - 1, min: 0, max: MWTableViewCellAppearance.count)) ?? .dark
        }
    }
    
    //MARK: Instance variables
    internal var appearance: MWTableViewCellAppearance = .dark
    
    internal var silent: Int = 0
    
    private var backgroundModificationAmount: CGFloat
    {
        get
        {
            let constant: CGFloat = 0.1
            
            switch appearance
            {
            case .dark:
                return constant
                
            case .light:
                return -constant
            }
        }
    }
    
    //MARK: - Inherited initializers from: UITableViewCell
    override init(style: UITableViewCellStyle, reuseIdentifier: String?)
    {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        
        _init()
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        
        _init()
    }
    
    //MARK: - Inherited functions from: UITableViewCell
    override func setSelected(_ selected: Bool, animated: Bool)
    {
        
    }
    
    //MARK: Instance functions
    private func _init()
    {
        self.selectionStyle = .none
    }
    
    func silently() -> MWTableViewCellNew
    {
        silent += 1
        return self
    }
    
    internal func silentSafely(_ safe: () -> ())
    {
        
    }
}

//MARK: -

enum MWTableViewCellAppearance: Int
{
    case dark
    case light
    
    static var count: Int
    {
        get
        {
            return light.hashValue + 1
        }
    }
}

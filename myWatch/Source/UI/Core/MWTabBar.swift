//
//  MWTabBar.swift
//  myWatch
//
//  Created by Máté on 2017. 06. 16.
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

/// A custom-designed tab bar for the application.
class MWTabBar: UITabBar
{
    //MARK: Inspectables
    
    /// The style of the tab bar in an Interface Builder-supported format.
    ///
    /// We use the number this variable holds to make an `MWTabBarStyle` instance out of it.
    ///
    /// Before we can use the variable, we have to clamp it, so that it does not get out of range of the `MWTabBarStyle` enumeration.
    /// - See: `count` in `MWTabBarStyle`
    /// - Also, see `_style` below for more details on the styles.
    @IBInspectable var style: Int = 1
    {
        didSet
        {
            _style = MWTabBarStyle(rawValue: MWUtil.clamp(style - 1, min: 0, max: MWTabBarStyle.count - 1))!
        }
    }
    
    /// Indicates whether the tab bar sould hide the titles of the items and center their icons.
    @IBInspectable var hidesTitles: Bool = false
    
    //MARK: Overriden variables
    
    /// Holds the items of the tab bar.
    ///
    /// This property has a custom setter, because whenever we set the ites of the tab bar, they have to be rearranged.
    ///
    /// The setter of this variable invokes function `setItems(_:animated:)`, which does all the rearranging work.
    ///
    /// Because of the setter, we need to specify a variable which holds the actual value of this property - in this case, that is `_items`.
    ///
    /// We return `_items` when getting this property's value.
    override var items: [UITabBarItem]?
    {
        get { return _items }
        set { setItems(newValue, animated: animated) }
    }
    
    //MARK: Instance variables
    
    /// Indicates whether any changes to the tab bar should be animated.
    var animated: Bool = false
    
    /// Holds the actual style of the tab bar which later will be used to determine which style should we draw.
    ///
    /// - See: `MWTabBarStyle` for more details on the styles.
    var _style: MWTabBarStyle = .system
    {
        didSet
        {
            _init()
        }
    }
    
    /// Holds the shadow layer for a custom tab bar style.
    private var shadowLayer: CALayer = CALayer()
    
    /// Holds the layer which the shadow should be clipped to for a custom tab bar style.
    private var clippingLayer: CALayer = CALayer()
    
    /// Holds the value of `items`.
    ///
    /// Because `items` has a custom setter which calls `setItems(_:animated)`, we need to create a property which stores its value, and prevents the application from being stuck in an infinite setter-loop - this is the purpose of this property.
    ///
    /// We return this variable when getting `items` (that is the reason why this property is never being read from in this file).
    ///
    /// - See: overriden property `items` for more details.
    private var _items: [UITabBarItem]?
    
    /// Holds the buttons for each item of the tab bar.
    ///
    /// Its layout is exactly like the `_items` array.
    private var buttons: [MWTabBarButton] = [MWTabBarButton]()
    
    /// Holds the corresponding button for the currently selected item of the tab bar.
    ///
    /// It is `nil` when no items are selected.
    private var selectedButtton: MWTabBarButton?
    
    /// A boolean which indicates whether the separator line (default shadow image) of the tab bar has already been removed.
    ///
    /// Only used if the style of the tab bar is set to custom.
    private var removedSeparatorLine: Bool = false
    
    //MARK: - Inherited initializers from: UITabBar
    override init(frame: CGRect)
    {
        super.init(frame: frame)
        
        //Initialize using our custom function
        _init()
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        
        //Initialize using our custom function
        _init()
    }
    
    //MARK: Inherited functions from: UITabBar
    override func layoutSubviews()
    {
        super.layoutSubviews()
        
        //Lay out subviews
        self.layoutIfNeeded()
    }
    
    override func layoutIfNeeded()
    {
        //Update/redraw the custom tab bar if the style is custom
        if(_style == .custom)
        {
            //Remove the separator line if we have not removed it already
            if(!removedSeparatorLine)
            {
                if let separatorLine = getSeparatorLine(for: self)
                {
                    separatorLine.isHidden = true
                    removedSeparatorLine = true
                }
            }
            
            //Update/redraw
            clippingLayer.frame = self.bounds.offsetBy(dx: 0.0, dy: -(self.bounds.height -- 30.0)).withSize(width: self.bounds.width, height: 30.0)
            
            let shadowPath = UIBezierPath(rect: self.bounds.scaleBy(width: -10.0, height: 0.0))
            shadowLayer.frame = clippingLayer.bounds.offsetBy(dx: 0.0, dy: clippingLayer.bounds.height)
            
            shadowLayer.shadowOffset = CGSize(width: 5.0, height: -6.0)
            shadowLayer.shadowPath = shadowPath.cgPath
        }
        
        //Lay out the items
        _setItems(animated: true)
        
        //Set the selected button if there none of them is selected
        self.selectedButtton ?= {
            self.buttons[0].isSelected = true
            self.selectedButtton = self.buttons[0]
        }
    }

    override func setItems(_ items: [UITabBarItem]?, animated: Bool)
    {
        //Check what should the function do
        MWUtil.nilcheck(items, not: {
            MWUtil.nilcheck(self.items, nil: {
                //Set intitial items of the tab bar / show the items of the tab bar
                self._setItems(items!, animated: animated)
            }, not: {
                if(self.items!.count > items!.count)
                {
                    //Add a new tab bar item
                    self.addItems(new: items!, animated: animated)
                }
                else if(self.items!.count < items!.count)
                {
                    //Remove a tab bar item
                    self.removeItems(new: items!, animated: animated)
                }
            })
        }) {
            MWUtil.nilcheck(self.items, not: {
                //Remove all items of the tab bar
                self.removeAllItems(animated)
            })
        }
    }
    
    //MARK: Instance functions
    
    /// Custom initializer for drawing specific tab bar styles.
    private func _init()
    {
        switch _style
        {
        case .system:
            break
            
        case .custom:
            self.layer.masksToBounds = false
            
            clippingLayer.frame = self.bounds.offsetBy(dx: 0.0, dy: -(self.bounds.height -- 30.0)).withSize(width: self.bounds.width, height: 30.0)
            clippingLayer.masksToBounds = true
            
            let shadowPath = UIBezierPath(rect: self.bounds.scaleByCentered(width: -10.0, height: 0.0))
            shadowLayer.frame = clippingLayer.bounds.offsetBy(dx: 0.0, dy: clippingLayer.bounds.height)
            
            shadowLayer.shadowColor = UIColor.black.cgColor
            shadowLayer.shadowRadius = 7.0
            shadowLayer.shadowOpacity = 0.5
            shadowLayer.shadowOffset = CGSize(width: 5.0, height: -6.0)
            shadowLayer.shadowPath = shadowPath.cgPath
            shadowLayer.masksToBounds = false
            
            clippingLayer.addSublayer(shadowLayer)
            self.layer.addSublayer(clippingLayer)
            
            break
        }
    }
    
    /// Sets/rearranges the items of the tab bar.
    ///
    /// - Parameters:
    ///   - items: The items that should initially be added to the tab bar. It is unnecccessary to provide, if using the function to rearrange.
    ///   - animated: A boolean which indicates that the process of adding the buttons initially or rearranging the existing ones should be animated.
    private func _setItems(_ items: [UITabBarItem]? = nil, animated: Bool, function: String = #function)
    {
        //Check if we are setting the item initially
        //If we are, an array of the new items must be provided, meaning it cannot be nil
        //If we are not, the items array is nil, because we already have the items set
        MWUtil.nilcheck(items, not: {
            //Initialize the "_items" array
            _items = [UITabBarItem]()
            
            //If we are, calculate one item's width based on the tab bar's width
            let width: CGFloat = self.frame.width / CGFloat(items!.count)
            
            //Iterate through the new items and add them
            for (i, item) in items!.enumerated()
            {
                //Create the button for the current item
                let button: MWTabBarButton = MWTabBarButton(tabBar: self, tabBarItem: item)
                
                //Create the frame for the button
                let frame: CGRect = CGRect(x: CGFloat(i) * width, y: 0.0, width: width, height: self.frame.height)
                
                //Check if we have to animate
                if(animated)
                {
                    //If we have to, prepare the button's alpha value
                    button.alpha = 0.0
                    
                    //Do the fade in animation
                    UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseOut, animations: {
                        button.alpha = 1.0
                    }, completion: nil)
                }
                
                //Set the button's frame
                button.frame = frame
                
                //Set the button's target for selecting
                button.addTarget(self, action: #selector(releaseSelect(sender:)), for: .touchUpInside)
                
                //Add the button to the tab bar
                self.addSubview(button)
                
                //Add the item to the tab bar items
                buttons.append(button)
                _items!.append(item)
            }
        }) {
            //If we are not, calculate one item's width based on the tab bar's width
            let width: CGFloat = self.frame.width / CGFloat(self.items!.count)
            
            //Iterate through the existing items and rearrange them
            for (i, _) in self.items!.enumerated()
            {
                //Get the button for the current item
                let button: MWTabBarButton = self.buttons[i]
                
                //Update the "hidesTitle" property of the buttons
                button.hidesTitle = self.hidesTitles
                
                //Create the frame for the button
                let frame: CGRect = CGRect(x: CGFloat(i) * width, y: 0.0, width: width, height: self.frame.height)
                
                //Check if we have to animate
                if(animated)
                {
                    //If we have to, do the animation where all items fly to their positions
                    UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseInOut, animations: {
                        button.frame = frame
                    }, completion: nil)
                }
                else
                {
                    //If we do no have to, simply set the button's frame
                    button.frame = frame
                }
            }
        }
    }
    
    /// Adds one or more items to the tab bar.
    ///
    /// - Parameters:
    ///   - items: The array which contains the item(s) that should be added to the tab bar.
    ///   - animated: A boolean which indicates that the process of adding the new button(s) and resizing the existing ones should be animated.
    private func addItems(new items: [UITabBarItem], animated: Bool)
    {
        //Calculate one item's width based on the tab bar's width
        let width: CGFloat = self.frame.width / CGFloat(items.count)
        
        for (i, item) in items.enumerated()
        {
            //Calculate the new frame of the button no matter what
            let frame: CGRect = CGRect(x: CGFloat(i) * width, y: 0.0, width: width, height: self.frame.height)
            
            //Check if the item exists in the current items of the tab bar
            guard let _ = self.items!.index(of: item) else
            {
                //If it does not, the current item is the new item, or one of the new items
                //This means that we have to create a new button for it.
                
                //Create a new button
                let button: MWTabBarButton = MWTabBarButton(tabBar: self, tabBarItem: item)
                
                //Check if we have to animate
                if(animated)
                {
                    //If we do, prepare the button for the animation
                    button.alpha = 0.0
                    
                    //Do the animation
                    UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseOut, animations: {
                        button.frame = frame
                        button.alpha = 1.0
                    }, completion: nil)
                }
                else
                {
                    //If we do not, simply set the button's frame
                    button.frame = frame
                }
                
                //Set the button's target for selecting
                button.addTarget(self, action: #selector(releaseSelect(sender:)), for: .touchUpInside)
                
                //Add this new item to the final array
                buttons.insert(button, at: i)
                _items!.insert(item, at: i)
                
                continue
            }
            
            //If the operation above succeeds, the item is among the current items of the tab bar, meaning we have to resize its button
            
            //Get the button for the current item
            let button: MWTabBarButton = buttons[i]
            
            //Check if we have to animate
            if(animated)
            {
                //If we do, do the animation
                UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseOut, animations: {
                    button.frame = frame
                }, completion: nil)
            }
            else
            {
                //If we do not, simply set the button's frame
                button.frame = frame
            }
        }
    }
    
    /// Removes one or more items from the tab bar.
    ///
    /// - Parameters:
    ///   - items: An array without the item(s) that has/have to be removed.
    ///   - animated: A boolean which indicates that the process of removing the button(s) and resizing the existing ones should be animated.
    private func removeItems(new items: [UITabBarItem], animated: Bool)
    {
        let width: CGFloat = self.frame.width / CGFloat(items.count)
        
        //An iterator for buttons that are not going to get removed
        //Required for frame calculation
        var _i: Int = 0
        
        for (i, item) in self.items!.enumerated()
        {
            //Get the button for the item
            let button: MWTabBarButton = buttons[i]
            
            //Try to get the index of the current item in the array which does not contain the item which should be removed
            //If the operation succeeds, the else block will not be executed, and we can assume that the current item is not the item which should be removed
            guard let _ = items.index(of: item) else
            {
                //If this block gets executed, we assume that the current item can not be found in the new array, which means the current item is the button which should be removed
                
                //Check if we have to animate
                if(animated)
                {
                    //If we do, do the animation
                    //As a completion after the animation has finished, we remove the button finally
                    UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseIn, animations: { 
                        button.frame = CGRect.zero
                        button.alpha = 0.0
                    }, completion: { (finished: Bool) in
                        button.removeFromSuperview()
                        self.buttons.remove(at: i)
                        self._items!.remove(at: i)
                    })
                }
                else
                {
                    //If we do not, simply remove the button
                    button.removeFromSuperview()
                    buttons.remove(at: i)
                    _items!.remove(at: i)
                }
                
                continue
            }
            
            //This segment should only be executed if the current item is not going to be removed
            
            //Check if we have to animate
            if(animated)
            {
                //If we do, do the animation
                UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseIn, animations: { 
                    button.frame = CGRect(x: CGFloat(_i) * width, y: 0.0, width: width, height: self.frame.height)
                }, completion: nil)
            }
            else
            {
                //If we do not, simply set the button's frame
                button.frame = CGRect(x: CGFloat(_i) * width, y: 0.0, width: width, height: self.frame.height)
            }
            
            //Increment the iterator for buttons that are not going to get removed
            _i += 1
        }
    }
    
    /// Removes all the items from the tab bar.
    ///
    /// - Parameter animated: A boolean which indicates that the process of removing all the buttons should be animated.
    private func removeAllItems(_ animated: Bool)
    {
        //Iterate through all the items
        for (i, _) in self.items!.enumerated()
        {
            //Get the button for the current item
            let button: MWTabBarButton = buttons[i]
            
            //Check if we have to animate
            if(animated)
            {
                //If we do, do the fade out animation and, as a completion, remove the item
                UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseIn, animations: { 
                    button.alpha = 0.0
                }, completion: { (finished: Bool) in
                    button.removeFromSuperview()
                    self.buttons.remove(at: i)
                    self._items!.remove(at: i)
                })
            }
            else
            {
                //If we do not, simply remove the item
                button.removeFromSuperview()
                buttons.remove(at: i)
                _items!.remove(at: i)
            }
        }
    }
    
    /// Called whenever a button is selected.
    ///
    /// - Parameter sender: The button which has just been selected.
    @objc private func releaseSelect(sender: MWTabBarButton)
    {
        //Deselect the currently selected button if there was one before
        selectedButtton?.isSelected = false
        
        //Select the new button
        sender.isSelected = true
        selectedButtton = sender
        
        //Inform the delegate that an item has been selected
        self.delegate?.tabBar?(self, didSelect: sender.tabBarItem)
    }
    
    /// Searches for a separator line in subviews of the given view
    ///
    /// - Parameter view: The view whose subviews the function should search for the separator line in.
    /// - Returns: The separator line (default shadow image) of the tab bar.
    private func getSeparatorLine(for view: UIView) -> UIImageView?
    {
        //Check if the current view is the separator line
        if(view is UIImageView && view.frame.height <= 1)
        {
            //If it is, return it as a "UIImageView"
            return view as? UIImageView
        }
        
        //If it is not, search for it in its subviews
        for subview in view.subviews
        {
            //For optimization puposes, we exclude the buttons from the search
            //(We are looking for a view with type "_UIBarBackground", but that is not a public view type available in UIKit.)
            if(!(subview is MWTabBarButton))
            {
                if let shadowImage = getSeparatorLine(for: subview)
                {
                    return shadowImage
                }
            }
        }
        
        return nil
    }
}

/// A custom button for a tab bar item on a tab bar.
class MWTabBarButton: UIControl
{
    /// A boolean indicating whether the button is selected.
    ///
    /// Whenever we set this value, based on the new value, the button either displays itself selected or unselected.
    override var isSelected: Bool
    {
        didSet
        {
            //Check if the button is currently selected
            if(isSelected)
            {
                //If it is, display the selected look
                
                //Set the image view
                imageView.silently().tintingColor = selectedColor
                
                UIView.transition(with: imageView, duration: 0.1, options: .transitionCrossDissolve, animations: {
                    self.imageView.image = self.selectedImage
                }, completion: nil)
                
                //Optionally, set the label
                MWUtil.nilcheck(label, not: { 
                    UIView.transition(with: label!, duration: 0.1, options: .transitionCrossDissolve, animations: {
                        self.label!.textColor = self.selectedColor
                    }, completion: nil)
                })
            }
            else
            {
                //If it is not, display the unselected look
                
                //Set the image view
                imageView.silently().tintingColor = unselectedColor
                
                UIView.transition(with: imageView, duration: 0.1, options: .transitionCrossDissolve, animations: {
                    self.imageView.image = self.image
                }, completion: nil)
                
                //Optionally, set the label
                MWUtil.nilcheck(label, not: {
                    UIView.transition(with: label!, duration: 0.1, options: .transitionCrossDissolve, animations: {
                        self.label!.textColor = self.unselectedColor
                    }, completion: nil)
                })
            }
        }
    }
    
    /// The frame of the button.
    ///
    /// Whenever we set its value, the button updates its layout, but only if the button has already been initialized.
    override var frame: CGRect
    {
        didSet
        {
            //Check if the button has already been initialized
            if(initilaized)
            {
                //If it has, layout immediately
                layoutIfNeeded()
            }
        }
    }
    
    /// The tab bar item corresponding to this button.
    var tabBarItem: UITabBarItem!
    
    /// A boolean which indicates whether the button should hide the item's title.
    var hidesTitle: Bool = false
    
    /// A boolean which indicates whether the button has already been initialized.
    ///
    /// If set to `true`, the button will lay out itself automatically whenever the frame changes.
    private var initilaized: Bool = false
    
    /// The image that sould show the image provided in the corresponding tab bar item.
    private var imageView: MWImageView!
    
    /// The image provided in the corresponding tab bar item for the unselected state.
    private var image: UIImage!
    
    /// The image provided in the corresponding tab bar item for the selected state.
    private var selectedImage: UIImage!
    
    /// The label showing the title of the tab bar item if the tab bar does not hide titles.
    private var label: UILabel?
    
    /// The color that the button should be tinted with if it is selected.
    private var selectedColor: UIColor!
    
    /// The color that the button should be tinted with if it is not selected.
    private var unselectedColor: UIColor!
    
    /// Makes an `MWTabBarItem` instance out of the given parameters.
    ///
    /// - Parameters:
    ///   - tabBar: The tab bar that this button belongs to.
    ///   - tabBarItem: The tab bar item which corresponds to this button.
    ///
    /// In the supercall, we provide a zero rectangle as the frame of the control.
    ///
    /// Laying out the actual frame of the button is done explicitly in `MWTabBar`.
    ///
    /// - See: `_setItems(_:animated:)` in `MWTabBar` for more details on the frame.
    init(tabBar: MWTabBar, tabBarItem: UITabBarItem)
    {
        super.init(frame: CGRect.zero)
        
        //Set the colors
        self.selectedColor = tabBar.tintColor
        self.unselectedColor = tabBar.unselectedItemTintColor ?? UIColor.lightGray
        
        //Create the image view
        self.imageView = MWImageView(frame: CGRect.zero)
        self.image = tabBarItem.image ?? MWAssets.Images.imageNoImage.getImage(in: Bundle(for: type(of: self)), traits: self.traitCollection)
        self.selectedImage = tabBarItem.selectedImage ?? tabBarItem.image
        
        imageView.silently().tintingColor = unselectedColor
        imageView.image = image
        
        imageView.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        
        //Add the image view
        self.addSubview(imageView)
        
        //Check if we are not hiding the title
        if(!tabBar.hidesTitles)
        {
            //If we are not, create the label
            self.label = UILabel()
            
            label!.font = tabBarItem.titleTextAttributes(for: .normal)?[NSFontAttributeName] as? UIFont ?? UIFont.systemFont(ofSize: 10.0)
            label!.text = tabBarItem.title
            label!.textColor = self.unselectedColor
            label!.textAlignment = .center

            label!.autoresizingMask = [.flexibleWidth, .flexibleHeight]
            
            //Add the label
            self.addSubview(label!)
        }
        
        //Set other variables
        self.tabBarItem = tabBarItem
        self.hidesTitle = tabBar.hidesTitles
        
        //Allow the button to lay out itself whenever its frame is changed
        initilaized = true
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        MWLError("MWTabBarButton has been initialized with initializer: \"required init?(coder:)\" Nothing will happen, creating an empty view...", module: nil)
        
        self.frame = CGRect.zero
    }
    
    /// Lays out the subviews of the button based on the button's frame.
    override func layoutIfNeeded()
    {
        //Set the frame for the image view based on whether we hide the title
        if(hidesTitle)
        {
            //Hide the title label
            label?.isHidden = true
            
            //If do hide the title, the image view's frame is centered both horizontally and vertically
            imageView.frame = CGRect(x: (self.frame.width - image.size.width) / 2, y: (self.frame.height - image.size.height) / 2, width: image.size.width, height: image.size.height)
        }
        else
        {
            //Show the title label
            label?.isHidden = false
            
            //If do not hide the title, the image view's frame is slightly offsetted from the top of the button
            imageView.frame = CGRect(x: (self.frame.width - image.size.width) / 2, y: 7, width: image.size.width, height: image.size.height)
            
            //Also, if we do not hide the title, set the label
            label!.sizeToFit()
            
            label!.frame = label!.frame.withPosition(x: (self.frame.width - label!.frame.width) / 2, y: self.frame.height - label!.frame.height - 1.0)
            
            self.clipsToBounds = false
        }
    }
}

/// The enumeration which holds all tab bar styles that currently exist in myWatch.
enum MWTabBarStyle: Int
{
    /// Case __system__ involves having a tab bar which looks like the system default
    case system
    
    /// Case __custom__ involves having a tab bar with a custom shadow and with the 1px separator line removed.
    case custom
    
    ///Holds the total amount of styles in this enumeration.
    ///
    ///This is required to make clamping the value given in `style` in `MWTabBar` possible.
    static var count: Int
    {
        return self.custom.hashValue + 1
    }
}


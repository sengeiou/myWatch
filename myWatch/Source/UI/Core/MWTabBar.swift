//
//  MWTabBar.swift
//  myWatch
//
//  Created by Máté on 2017. 06. 16.
//  Copyright © 2017. theMatys. All rights reserved.
//

import UIKit

class MWTabBar: UITabBar
{
    override var items: [UITabBarItem]?
    {
        get { return _items }
        set { setItems(newValue, animated: animated) }
    }
    
    var animated: Bool = false
    
    private var _items: [UITabBarItem]?
    private var buttons: [MWTabBarButton] = [MWTabBarButton]()
    private var selectedButtton: MWTabBarButton?
    
    override init(frame: CGRect)
    {
        super.init(frame: frame)
        initialize()
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        initialize()
    }
    
    //This function is called whenever we change the contents of "items" in UITabBar or whenever we need to update the layout of the items.
    //It is called for the first time when we give "items" an inital value.
    //It is called for the second time when the constraints of the tab bar have been updated and it achieves its final frame. In this call, we lay out the initial items of the tab bar correctly.
    //Later, the function may be called when we add or remove an item from the tab bar after the intialization.
    override func setItems(_ items: [UITabBarItem]?, animated: Bool)
    {
        MWUtil.execute(ifNotNil: items) {
            //This is for the first call when we initialize the buttons for the tab bar items.
            if(self.buttons.count == 0)
            {
                self.makeInitialButtons(items!)
            }
            //This is for the second call when we lay out the buttons correctly.
            else if(items!.count == self.buttons.count)
            {
                self.layoutButtons()

                self.buttons[0].isSelected = true
                self.selectedButtton = self.buttons[0]
            }
            //This is for the optional call when a new item is added to the tab bar.
            //In this scenario, the number of items in the parameter "items" must be greater than the number of items in "buttons", because the newest item does not have a corresponding button in the "buttons" array.
            //Because the "(self.)items" array is an optional and because the "buttons" is exactly laid out like it (but without making it an optional), it is better to check with that instead of "(self.)items"
            else if(items!.count > self.buttons.count)
            {
                for (i, item) in items!.enumerated()
                {
                    if(self.items == nil || !self.items!.contains(item) && item is MWTabBarItem)
                    {
                        let _item: MWTabBarItem = item as! MWTabBarItem
                        
                        self.addNewButton(_item, at: i)
                        
                        //We lay out the buttons, because by adding a new item, we reduce the original frame of the existing buttons.
                        if(animated)
                        {
                            self.layoutButtons(.adding, at: i)
                        }
                        else
                        {
                            self.layoutButtons()
                        }
                    }
                }
            }
            //This is for the optional call when an item is removed from the bar.
            //In this scenario, the number of items in the parameter "items" must be lower than the number of items in "buttons", because an item was removed, but its button still remains in the "buttons" array.
            //Because the "(self.)items" array is an optional and because the "buttons" is exactly laid out like it (but without making it an optional), it is better to check with that instead of "(self.)items"
            else if(items!.count < self.buttons.count)
            {
                MWUtil.execute(ifNotNil: self.items, execution: {
                    for (i, item) in self.items!.enumerated()
                    {
                        if(!items!.contains(item))
                        {
                            let correspondingButton: MWTabBarButton = self.buttons[i]
                            
                            if(animated)
                            {
                                self.layoutButtons(.removing, at: i)
                            }
                            else
                            {
                                correspondingButton.removeFromSuperview()
                                self.buttons.remove(at: i)
                                
                                self.layoutButtons()
                            }
                        }
                    }
                })
            }
        }
        
        _items = items
    }
    
    private func initialize()
    {
        
    }
    
    private func makeInitialButtons(_ items: [UITabBarItem])
    {
        for item in items
        {
            if(item is MWTabBarItem)
            {
                let _item: MWTabBarItem = item as! MWTabBarItem
                let button: MWTabBarButton = MWTabBarButton(tabBar: self, tabBarItem: _item)
                
                button.addTarget(self, action: #selector(highlight(sender:)), for: .touchDown)
                button.addTarget(self, action: #selector(releaseSelect(sender:)), for: .touchUpInside)
                button.addTarget(self, action: #selector(releaseNoSelect(sender:)), for: .touchUpOutside)
                button.addTarget(self, action: #selector(releaseNoSelect(sender:)), for: .touchDragExit)
                
                self.addSubview(button)
                self.buttons.append(button)
            }
        }
    }
    
    private func layoutButtons(_ modification: MWTabBarModification? = nil, at: Int? = nil)
    {
        let itemWidth: CGFloat = self.frame.width / CGFloat(modification != nil && modification == .removing ? buttons.count - 1 : buttons.count)
        let yOffset: CGFloat = CGFloat(1) / UIScreen.main.scale
        
        if(!animated)
        {
            for (i, button) in self.buttons.enumerated()
            {
                button.frame = CGRect(x: CGFloat(i) * itemWidth, y: yOffset, width: itemWidth, height: self.frame.height - yOffset)
                button.layout()
            }
        }
        else
        {
            MWUtil.execute(ifNotNil: modification, execution: {
                MWUtil.execute(ifNotNil: at, execution: { 
                    switch modification!
                    {
                    case .adding:
                        for (i, button) in self.buttons.enumerated()
                        {
                            if(i == at!)
                            {
                                //Hide the button and make its final frame
                                button.alpha = 0.0
                                button.frame = CGRect(x: CGFloat(i) * itemWidth, y: yOffset, width: itemWidth, height: self.frame.height - yOffset)
                                
                                //With the final frame, lay out the button
                                button.layout()
                                
                                //Execute the animation
                                UIView.animate(withDuration: 0.15, delay: 0.35, options: .curveEaseOut, animations: {
                                    button.alpha = 1.0
                                }, completion: nil)
                            }
                            else
                            {
                                UIView.animate(withDuration: 0.35, delay: 0.0, options: .curveEaseOut, animations: { 
                                    button.frame = CGRect(x: CGFloat(i) * itemWidth, y: yOffset, width: itemWidth, height: self.frame.height - yOffset)
                                }, completion: nil)
                            }
                        }
                        
                        break
                        
                    case .removing:
                        for (i, button) in self.buttons.enumerated()
                        {
                            if(i == at!)
                            {
                                UIView.animate(withDuration: 0.15, delay: 0.0, options: .curveEaseIn, animations: {
                                    button.alpha = 0.0
                                }, completion: nil)
                            }
                            else
                            {
                                UIView.animate(withDuration: 0.35, delay: 0.15, options: .curveEaseIn, animations: {
                                    button.frame = CGRect(x: CGFloat(i < at! ? i : i - 1) * itemWidth, y: yOffset, width: itemWidth, height: self.frame.height - yOffset)
                                }, completion: nil)
                            }
                        }
                        
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5, execute: {
                            self.buttons[at!].removeFromSuperview()
                            self.buttons.remove(at: at!)
                        })
                        
                        break
                    }
                }, elseExecution: {
                    for (i, button) in self.buttons.enumerated()
                    {
                        button.frame = CGRect(x: CGFloat(i) * itemWidth, y: yOffset, width: itemWidth, height: self.frame.height - yOffset)
                    }
                    
                    MWLDebug("No animations happened, because there was no \"at\" parameter specified.", module: nil)
                })
            }, elseExecution: { 
                for (i, button) in self.buttons.enumerated()
                {
                    button.frame = CGRect(x: CGFloat(i) * itemWidth, y: yOffset, width: itemWidth, height: self.frame.height - yOffset)
                }
                
                MWLDebug("No animations happened, because there was no modification type specified.", module: nil)
            })
        }
    }
    
    private func addNewButton(_ item: MWTabBarItem, at: Int)
    {
        let button: MWTabBarButton = MWTabBarButton(tabBar: self, tabBarItem: item)
        
        button.addTarget(self, action: #selector(highlight(sender:)), for: .touchDown)
        button.addTarget(self, action: #selector(releaseSelect(sender:)), for: .touchUpInside)
        button.addTarget(self, action: #selector(releaseNoSelect(sender:)), for: .touchUpOutside)
        button.addTarget(self, action: #selector(releaseNoSelect(sender:)), for: .touchDragExit)
        
        self.addSubview(button)
        self.buttons.insert(button, at: at)
    }
    
    @objc private func highlight(sender: MWTabBarButton)
    {
        sender.highlight(sender: sender)
    }
    
    @objc private func releaseSelect(sender: MWTabBarButton)
    {
        sender.release(sender: sender)
        
        selectedButtton?.isSelected = false
        sender.isSelected = true
        selectedButtton = sender
        
        self.delegate?.tabBar?(self, didSelect: sender.tabBarItem)
    }
    
    @objc private func releaseNoSelect(sender: MWTabBarButton)
    {
        sender.release(sender: sender)
    }
}

class MWTabBarItem: UITabBarItem
{
    @IBInspectable var hidesTitle: Bool = false
    var noImage: Bool = false
    
    override init()
    {
        super.init()
        makeEmpty()
    }
    
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        make()
    }
    
    private func makeEmpty()
    {
        noImage = true
        self.title = "???"
    }
    
    private func make()
    {
        MWUtil.execute(ifNil: self.image) { 
            self.noImage = true
        }
        
        MWUtil.execute(ifNil: self.title) {
            self.title = "???"
        }
    }
}

class MWTabBarButton: UIControl
{
    override var isSelected: Bool
    {
        didSet
        {
            if(isSelected)
            {
                imageView.autoUpdate = false
                imageView.tintingColor = selectedColor
                
                UIView.transition(with: imageView, duration: 0.1, options: .transitionCrossDissolve, animations: {
                    self.imageView.image = self.selectedImage
                }, completion: nil)
            }
            else
            {
                imageView.autoUpdate = false
                imageView.tintingColor = unselectedColor
                
                UIView.transition(with: imageView, duration: 0.1, options: .transitionCrossDissolve, animations: {
                    self.imageView.image = self.image
                }, completion: nil)
            }
        }
    }
    
    var tabBarItem: MWTabBarItem!
    
    private var imageView: MWImageView!
    private var image: UIImage!
    private var selectedImage: UIImage!
    
    private var label: UILabel?
    
    private var selectedColor: UIColor!
    private var unselectedColor: UIColor!
    
    private var originalImageViewFrame: CGRect?
    private var originalLabelFrame: CGRect?
    
    //This initializer is the only one that gets used.
    //It will create a button from the given parameters:
    // - tabBar: Required to get the tint colors.
    // - tabBarItem: Needed to aquire the image and the title of the item.
    
    //The actual setup is done in function "make(tabBar:tabBarItem:)"
    
    //As you can see, the frame we provide in the supercall is "CGRect.zero"
    //This is, because we the actual frame of this view is given in "layoutButtons()" in MWTabBar.
    //For more information, see "setItems(_:animated:)" in MWTabBar.
    init(tabBar: UITabBar, tabBarItem: MWTabBarItem)
    {
        super.init(frame: CGRect.zero)
        initialize(tabBar: tabBar, tabBarItem: tabBarItem)
    }
    
    //This initializer is only implemented, because it is REQUIRED to be implemented.
    //Normally, this initializer will never get called, because we never instantiate this control through Interface Builder.
    //If it does get called however, it will create an entirely empty view, and will display an error message, that this initializer was not meant to be used.
    required init?(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        MWLError("MWTabBarButton has been initialized with initializer: \"required init?(coder:)\" Nothing will happen, creating an empty view...", module: nil)
    }
    
    private func initialize(tabBar: UITabBar, tabBarItem: MWTabBarItem)
    {
        //Construct variables
        self.imageView = MWImageView()
        self.image = tabBarItem.image ?? MWAssets.Images.imageNoImage.getImage(inBundle: Bundle(for: type(of: self)), traits: self.traitCollection)
        self.selectedImage = tabBarItem.selectedImage ?? tabBarItem.image
        
        if(!tabBarItem.hidesTitle)
        {
            self.label = UILabel()
        }
        
        self.selectedColor = tabBar.tintColor
        self.unselectedColor = tabBar.unselectedItemTintColor ?? UIColor.lightGray
        
        self.tabBarItem = tabBarItem
    }
    
    func layout()
    {
        //Make the image view
        // - Set the image and the tinting color.
        imageView.autoUpdate = false
        
        imageView.tintingColor = unselectedColor
        imageView.image = image
        
        // - Set the positioning
        if(tabBarItem.hidesTitle)
        {
            imageView.frame = CGRect(x: (self.frame.width - image.size.width) / 2, y: (self.frame.height - image.size.height) / 2, width: image.size.width, height: image.size.height)
        }
        else
        {
            imageView.frame = CGRect(x: (self.frame.width - image.size.width) / 2, y: 5, width: image.size.width, height: image.size.height)
        }
        
        self.addSubview(imageView)
    }
    
    fileprivate func highlight(sender: MWTabBarButton)
    {
        originalImageViewFrame = imageView.frame
        
        UIView.animate(withDuration: 0.1, delay: 0.0, options: .curveEaseOut, animations: {
            self.imageView.frame = CGRect(x: (self.frame.width - (0.75 * self.imageView.frame.width)) / 2, y: (self.frame.height - (0.75 * self.imageView.frame.height)) / 2, width: 0.75 * self.imageView.frame.width, height: 0.75 * self.imageView.frame.height)
        }, completion: nil)
    }
    
    fileprivate func release(sender: MWTabBarButton)
    {
        UIView.animate(withDuration: 0.1, delay: 0.0, options: .curveEaseOut, animations: {
            self.imageView.frame = self.originalImageViewFrame ?? CGRect(x: (self.frame.width - 25) / 2, y: (self.frame.height - 25) / 2, width: 25, height: 25)
        }, completion: nil)
    }
}

enum MWTabBarModification
{
    case adding
    case removing
}


import {NextFunction, Request, Response} from "express";

export default interface IFreeTextController{
    addFreeText(req: Request, res: Response, next: NextFunction);
}
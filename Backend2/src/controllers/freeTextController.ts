import {Inject, Service} from "typedi";
import IFreeTextController from "./IControllers/IFreeTextController";
import config from "../../config";
import IMedicalRecordFreeTextDTO from "../dto/IMedicalRecordFreeTextDTO";
import IFreeTextService from "../services/IServices/IFreeTextService";


@Service()

export default class FreeTextController implements IFreeTextController {

    constructor(@Inject(config.services.freeText.name) private freeTextServiceInstance: IFreeTextService) {
    }

     public async addFreeText(req: any, res: any): Promise<any> {
        let freeText: IMedicalRecordFreeTextDTO = req.body;

        try {
            await this.freeTextServiceInstance.addFreeText(freeText);

            res.status(200).json({
                message: 'Comment Added Successfully'
            });

        } catch (error) {
            console.error('Error adding comment:', error.message);

        }
    }


}
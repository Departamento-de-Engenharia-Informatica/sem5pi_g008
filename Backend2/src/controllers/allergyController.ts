import {Inject, Service} from "typedi";
import IAllergyController from "./IControllers/IAllergyController";
import config from "../../config";
import IAllergyService from "../services/IServices/IAllergyService";
import IAllergyDTO from "../dto/IAllergyDTO";

@Service()
export default class AllergyController implements IAllergyController {
  constructor(
    @Inject(config.services.allergy.name) private allergyServiceInstance: IAllergyService
  ) {}

  public async createAllergy(req: any, res: any): Promise<any> {

    let allergy: IAllergyDTO = req.body;

    try {
      await this.allergyServiceInstance.createAllergy(allergy);

      res.status(200).json({
        message: 'Allergy created successfully'
      });

    } catch (error) {
      console.error('Error creating allergy:', error.message);

      res.status(500).json({
        message: 'Error creating allergy',
        error: error.message
      });
    }
  }
}

import {Inject, Service} from "typedi";
import IAllergyController from "./IControllers/IAllergyController";
import config from "../../config";
import IAllergyService from "../services/IServices/IAllergyService";
import axios from "axios";

@Service()
export default class AllergyController implements IAllergyController {
  constructor(
    @Inject(config.services.allergy.name) private allergyServiceInstance: IAllergyService
  ) {}

  public async createAllergy(req: any, res: any): Promise<any> {
    return 1;
  }
}

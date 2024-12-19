import {Inject, Service} from "typedi";
import IAllergyService from "./IServices/IAllergyService";
import config from "../../config";
import IAllergyRepo from "./IRepos/IAllergyRepo";
import {AllergyMap} from "../mappers/AllergyMap";

@Service()
export default class AllergyService implements IAllergyService {
  constructor(
      @Inject(config.repos.allergy.name) private allergyRepo: IAllergyRepo
  ) {}

  public async createAllergy(allergy: any): Promise<any> {

      if(allergy.domainId === undefined) {
        await this.allergyRepo.save(allergy);
      } else {
        await this.allergyRepo.save(allergy, allergy.domainId);
      }
  }

  public async getAllAllergies(): Promise<any> {
    let aux = await this.allergyRepo.getAll();
    let dtoArray = new Array(aux.length);
    for(let i = 0; i < aux.length; i++) {
      dtoArray[i] = AllergyMap.toDTO(aux[i]);
    }
    return dtoArray;
  }

}

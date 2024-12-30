import {Inject, Service} from "typedi";
import IAllergyService from "./IServices/IAllergyService";
import config from "../../config";
import IAllergyRepo from "./IRepos/IAllergyRepo";
import {AllergyMap} from "../mappers/AllergyMap";
import {Allergy} from "../domain/Allergy/Allergy";
import IAllergyDTO from "../dto/IAllergyDTO";
import {Code} from "../domain/Shared/code";
import {Designation} from "../domain/Shared/designation";
import {Description} from "../domain/Shared/description";

@Service()
export default class AllergyService implements IAllergyService {
  constructor(
      @Inject(config.repos.allergy.name) private allergyRepo: IAllergyRepo
  ) {}

  public async createAllergy(allergyDTO: IAllergyDTO): Promise<any> {

    const allergyProps = {
      code: Code.create(allergyDTO.code).getValue(),
      designation: Designation.create(allergyDTO.designation).getValue(),
      description: Description.create(allergyDTO.description).getValue(),
      effects: allergyDTO.effects
    };

    const allergyDomain = Allergy.create(allergyProps);

      if(allergyDTO.domainId === undefined) {
        await this.allergyRepo.save(allergyDomain.getValue());
      } else {
        await this.allergyRepo.save(allergyDomain.getValue(), allergyDTO.domainId);
      }
  }

  public async updateAllergyDesignation(id: string, designation: string): Promise<any> {
    const allergy = await this.allergyRepo.getByDomainId(Number.parseInt(id));
    allergy.designation = Designation.create(designation).getValue();
    await this.allergyRepo.updateUsingDomainId(allergy, 'designation');
    return AllergyMap.toDTO(allergy);
  }

  public async updateAllergyDescription(id: string, description: string): Promise<any> {
    const allergy = await this.allergyRepo.getByDomainId(Number.parseInt(id));
    allergy.description = Description.create(description).getValue();
    await this.allergyRepo.updateUsingDomainId(allergy, 'description');
    return AllergyMap.toDTO(allergy);
  }

  public async updateAllergyEffects(id: string, effects: string[]): Promise<any> {
    const allergy = await this.allergyRepo.getByDomainId(Number.parseInt(id));
    allergy.updateEffects(effects);
    await this.allergyRepo.updateUsingDomainId(allergy,'effects');
    return AllergyMap.toDTO(allergy);
  }
  
    public async getAllAllergies(): Promise<any> {
    let aux = await this.allergyRepo.getAll();
    let dtoArray = new Array(aux.length);
    for(let i = 0; i < aux.length; i++) {
      dtoArray[i] = AllergyMap.toDTO(aux[i]);
    }
    return dtoArray;
  }

  public async searchAllergies(allergy: string): Promise<any>{
    let aux = await this.allergyRepo.search(allergy);
    return AllergyMap.toDTO(aux);
  }

  public async getAllergyFromId(id: string): Promise<any> {
    return await this.allergyRepo.getById(id);
  }

}

import {Inject, Service} from "typedi";
import IAllergyService from "./IServices/IAllergyService";
import config from "../../config";
import IAllergyRepo from "./IRepos/IAllergyRepo";
import axios from 'axios';

@Service()
export default class AllergyService implements IAllergyService {
  constructor(
      @Inject(config.repos.allergy.name) private allergyRepo: IAllergyRepo
  ) {}
}

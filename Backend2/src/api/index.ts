import { Router } from 'express';
import allergy from './routes/allergyRoute';
import medicalConditions from './routes/medicalConditionRoute';

export default () => {
	const app = Router();

  allergy(app);

  medicalConditions(app);
  
	return app
}
